#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <cctype>
#include <cstdlib>
#include <memory>
#include <boost/icl/interval_map.hpp>
#include <srk31/algorithm.hpp>
#include <srk31/ordinal.hpp>
#include <cxxgen/tokens.hpp>
#include <dwarfpp/lib.hpp>
#include <dwarfpp/frame.hpp>
#include <dwarfpp/regs.hpp>

#include "stickyroot.hpp"
#include "frame-element.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::map;
using std::make_shared;
using std::ios;
using std::ifstream;
using std::dynamic_pointer_cast;
using boost::optional;
using std::ostringstream;
using std::set;
using namespace dwarf;
//using boost::filesystem::path;
using dwarf::core::iterator_base;
using dwarf::core::iterator_df;
using dwarf::core::iterator_sibs;
using dwarf::core::type_die;
using dwarf::core::subprogram_die;
using dwarf::core::compile_unit_die;
using dwarf::core::member_die;
using dwarf::core::with_data_members_die;
using dwarf::core::variable_die;
using dwarf::core::program_element_die;
using dwarf::core::with_static_location_die;
using dwarf::core::with_dynamic_location_die;
using dwarf::core::address_holding_type_die;
using dwarf::core::array_type_die;
using dwarf::core::type_chain_die;
using dwarf::encap::loc_expr;

using namespace dwarf::lib;

namespace allocs {
namespace tool {

bool operator<(const frame_element& x,
		       const frame_element& y)
{
	/* This means frame_elements for the same var will always
	 * compare equal, so we can't distinguish different effective exprs
	 * of the same var. This would matter if we wanted to record them
	 * at the same PC, but we don't. Still feels a bit fishy.
	 * However, note that we can't just use the effective expression
	 * because it's totally possible for the same expression to be
	 * used for multiple local vars at the same time. */
	return (x.m_local < y.m_local)
		|| ((x.m_local == y.m_local) && x.m_caller_regnum < y.m_caller_regnum)
		|| ((x.m_local == y.m_local) && (x.m_caller_regnum == y.m_caller_regnum) &&
			x.effective_expr_piece.copy() < y.effective_expr_piece.copy())
	//	|| ((x.first == y.first) && x.second.first.offset_here() == y.second.first.offset_here()
	//		&& x.second.first.second.second < y.second.first.second.second );
	;
}

bool operator==(const frame_element& x,
		       const frame_element& y)
{
	return !(x < y) && !(y < x);
}

bool frame_element::location_depends_on_register() const
{
	if (!has_location()) return false;
	auto& spec = m_local ? m_local.spec_here() : spec::DEFAULT_DWARF_SPEC;
	bool saw_register = false;
	for (auto i_instr = effective_expr_piece.first; i_instr != effective_expr_piece.second;
		++i_instr)
	{
		if (spec.op_reads_register(i_instr->lr_atom)
			|| /* CFA counts as a register */ i_instr->lr_atom == DW_OP_call_frame_cfa)
		{ saw_register = true; break; }
	}
	return saw_register;
}

optional<Dwarf_Signed> frame_element::has_fixed_offset_from_frame_base() const
{
	typedef optional<Dwarf_Signed> ret_t;
	// if we have a varying location, we don't have a fixed offset
	if (this->location_depends_on_register()) return ret_t();
	// v-- can't do this -- would be circular.
	//if (this->has_varying_location()) return ret_t();
	/* FIXME: so what happens if our location is not register-based, but is
	 * varying. E.g. imagine a location specified by a loc expr that does an
	 * indirection -- say a handle-style indirection where the handle
	 * lives at a global variable and it's the referent of the handle that is
	 * defined as the location. How would that pop out? Currently we'd get
	 * ADDRESS coming out and it would look fine. Really we need to scan the
	 * loc expr for anything that consumes input not on the stack... e.g. reading
	 * the pointer, in the handle-style case. FIXME: generalise/add to
	 * location_depends_on_register as appropriate... loc_expr_reads_machine_context? */
	if (this->has_fixed_register()) return ret_t();
	if (this->has_value_function()) return ret_t();
	if (this->has_implicit_value()) return ret_t();
	/* OK, see if we get an address out. */
	try
	{
		dwarf::expr::evaluator e(effective_expr_piece.copy_as_if_whole(),
			m_local.spec_here(),
			/* fb */ 0L,
			{ 0 } /* push zero (a.k.a. the frame base) onto the initial stack */);
		// FIXME: really want to push the offset of the stack pointer from the frame base
		// so that what comes out is a sp-relative offset?
		switch (e.tos_state())
		{
			case dwarf::expr::evaluator::NAMED_REGISTER:
				/* OK, the value lives in a register... we should have caught this! */
				std::clog << "Troublesome expression: " << effective_expr_piece.copy_as_if_whole() << endl;
				assert(false);
				break;
			case dwarf::expr::evaluator::ADDRESS: // the good one
			{
				// cerr << "Found on-stack location (fb + " << frame_offset << ") for fp/var " << *i_el 
				// << "in the vaddr range " 
				// << std::hex << i_int->first << std::dec << endl;
				return optional<Dwarf_Signed>(e.tos(dwarf::expr::evaluator::ADDRESS)); // may *not* be value; must be loc
			}
			default: // IMPLICIT_POINTER or VALUE
#if 0
				if (debug_out > 1)
				{
#endif
					cerr << "Top-of-stack indicates non-address-or-register result" << std::endl;
#if 0
				}
#endif
				assert(false);
				break;
		}
	}
	catch (dwarf::lib::No_entry)
	{
		/* Not much can cause this, since we scanned for registers.
		 * One thing would be a local whose location gives DW_OP_stack_value,
		 * i.e. it has only a debug-time-computable value but no location in memory,
		 * or DW_OP_implicit_pointer, i.e. it points within some such value. */
#if 0
		if (debug_out > 1)
		{
#endif
			cerr << "Warning: failed to locate non-register-located local/fp "
#if 0
				<< "in the vaddr range " 
				<< std::hex << the_int << std::dec
				<< ": "
#endif
				<< m_local.summary() << endl;
#if 0
		}
#endif
		return ret_t();
	}
	catch (dwarf::expr::Not_supported)
	{
		cerr << "Warning: unsupported DWARF opcode when computing location for fp: "
			<< m_local.summary() << endl;
		return ret_t();
	}
	catch (...)
	{
		cerr << "Warning: something strange happened when computing location for fp: " 
			<< m_local.summary() << endl;
		return ret_t();
	}
	assert(false);
}


/* FIXME: put these helpers somewhere better, i.e. on the loc_expr class */
static bool
locexpr_is_for_all_vaddrs(const loc_expr& locexpr)
{
	return locexpr.lopc == 0 && 0 == locexpr.hipc
		|| locexpr.lopc == 0 && locexpr.hipc == std::numeric_limits<Dwarf_Off>::max();
}
static bool
locexpr_is_base_address_selector(const loc_expr& locexpr, root_die& root)
{
	// FIXME: disgusting hack for detecting base address selection entries
	// -- should be sensitive to DWARF word size
	return locexpr.lopc == 0xffffffffffffffffULL
				|| locexpr.lopc == 0xffffffffUL;
}

vector<boost::icl::discrete_interval<Dwarf_Addr> >
intervals_for_local_var_locexpr(const loc_expr& locexpr, iterator_df<subprogram_die> i_subp,
	iterator_df<with_dynamic_location_die> i_dyn, sticky_root_die& root)
{
	vector<boost::icl::discrete_interval<Dwarf_Addr> > out;
	if (locexpr_is_for_all_vaddrs(locexpr))
	{
		/* we will just add the intervals of the containing subprogram */
		auto subp_intervals = i_subp->file_relative_intervals(root, nullptr, nullptr);
			//pc_intervals_by_subprogram[i_subp]; // re-use cached
		for (auto i_subp_int = subp_intervals.begin();
			i_subp_int != subp_intervals.end(); 
			++i_subp_int)
		{
			out.push_back(i_subp_int->first);
			cerr << "Borrowing vaddr ranges of " << *i_subp
				<< " for dynamic-location " << *i_dyn << endl;
			// print_sp_expr(root, our_interval.lower(), our_interval.upper());
			// print_intervals_stats(root, i_subp, subp_frame_intervals);
		}
	}
	else /* we have nonzero lopc and/or hipc */
	{
		/* We *do* have to adjust these by cu_base, because 
		 * we're getting them straight from the location expression. */
		auto opt_cu_base = i_subp.enclosing_cu()->get_low_pc();
		if (!opt_cu_base)
		{
			cerr << "ERROR: subprogram " << *i_subp 
				<< " -- in CU with no base address (CU: "
				<< *i_subp.enclosing_cu()
				<< ")" << endl;
			abort();
			// FIXME: can CUs use DW_AT_ranges instead? should handle this if so
		}
		auto our_interval = boost::icl::discrete_interval<Dwarf_Addr>::right_open(
			locexpr.lopc + opt_cu_base->addr, locexpr.hipc + opt_cu_base->addr
		); 
		out.push_back(our_interval);

		/* assert sane interval */
		assert(our_interval.lower() < our_interval.upper());
		/* assert sane size -- no bigger than biggest sane function */
		assert(our_interval.upper() - our_interval.lower() < 1024*1024);
		// print_sp_expr(root, our_interval.lower(), our_interval.upper());
		// print_intervals_stats(root, i_subp, subp_frame_intervals);
	}
	return out;
}

set< pair< boost::icl::discrete_interval<Dwarf_Addr>, frame_element > >
frame_element::local_elements_for(iterator_df<with_dynamic_location_die> d,
	iterator_df<subprogram_die> i_subp,
	sticky_root_die& root)
{
	set< pair< boost::icl::discrete_interval<Dwarf_Addr>, frame_element > > out;
	assert(!d->location_requires_object_base());

	/* enumerate the vaddr ranges of this DIE
	 * -- note that some DIEs will be "for all vaddrs" */
	auto var_loclist = d->get_dynamic_location();
	// rewrite the loclist to use the CFA/frame_base maximally
#ifdef DEBUG
	cerr << "Saw loclist " << var_loclist << endl;
#endif
	/* FIXME: what if root.get_frame_section() does not
	 * return the right frame section? Really we need
	 * sticky_root_die to do the thing we do in frametypes2
	 * of searching for the contentful frame section.
	 * find_nonempty_frame_section(). */
	var_loclist = encap::rewrite_loclist_in_terms_of_cfa(
		var_loclist,
		root.get_frame_section(),
		dwarf::spec::opt<const encap::loclist&>() /* opt_fbreg */
	);
#ifdef DEBUG
	cerr << "Rewrote to loclist " << var_loclist << endl;
#endif

	// for each of this variable's intervals, create a frame_element
	int interval_index = 0;
	for (auto i_locexpr = var_loclist.begin(); 
		i_locexpr != var_loclist.end(); ++i_locexpr)
	{
		if (locexpr_is_base_address_selector(*i_locexpr, root))
		{
			// we got a base address selection entry -- not handled yet
			assert(false);
			abort();
		}
		if (i_locexpr->lopc == i_locexpr->hipc && i_locexpr->hipc != 0) continue; // skip empties
		if (i_locexpr->hipc <  i_locexpr->lopc)
		{
			cerr << "Warning: lopc (0x" << std::hex << i_locexpr->lopc << std::dec
				<< ") > hipc (0x" << std::hex << i_locexpr->hipc << std::dec << ")"
				<< " in " << *d << endl;
			continue;
		}
		// handle "for all vaddrs" entries... may have multiple intervals,
		// if a function is not contiguous.
		if (locexpr_is_for_all_vaddrs(*i_locexpr))
		{
			// if we have a "for all vaddrs" entry, we should be the only index
			assert(interval_index == 0);
			assert(i_locexpr + 1 == var_loclist.end());
		}
		vector<boost::icl::discrete_interval<Dwarf_Addr> > our_intervals
		= intervals_for_local_var_locexpr(*i_locexpr, i_subp, d, root);
		for (auto i_our_interval = our_intervals.begin();
			i_our_interval != our_intervals.end();
			++i_our_interval)
		{
			auto& our_interval = *i_our_interval;

			/* assert sane interval */
			assert(our_interval.lower() < our_interval.upper());
			/* assert sane size -- no bigger than biggest sane function -- say 1MB */
			assert(our_interval.upper() - our_interval.lower() < 1024*1024);

			// add each piece
			shared_ptr<loc_expr> p_expr = make_shared<loc_expr>(*i_locexpr);
			auto pieces = p_expr->all_pieces();
			
			unsigned cur_bit_offset = 0;
			for (auto i_piece = pieces.begin(); i_piece != pieces.end(); ++i_piece)
			{
				out.insert(make_pair(our_interval, frame_element(d, *i_piece, p_expr)));
			}
		}
	} // end for each locexpr
	return out;
}

/* FIXME: put this helper somewhere else */
iterator_df<subprogram_die>
unique_subprogram_at(
	subprogram_vaddr_interval_map_t const& subprograms,
	Dwarf_Addr pc)
{
	iterator_df<subprogram_die> one_seen = iterator_base::END;
	bool unique = true;
	auto i = subprograms.find(pc);
	if (i == subprograms.end()) cerr << "No subprogram found at 0x" << std::hex << pc << std::dec
		<< endl;
	else cerr << "First subprogram found at 0x" << std::hex << pc << std::dec
		<< " has interval " << std::hex << i->first << std::dec << endl;
	unsigned nseen = 0;
	for (; i != subprograms.end() && i->first.lower() <= pc; ++i, ++nseen)
	{
		if (i->second.size() == 0) continue;
		if (i->second.size() > 1 || one_seen)
		{
			unique = false;
			cerr << "Over interval (" << std::hex << i->first.lower()
				<< ", " << i->first.upper() << "]" << std::dec
				<< ", found multiple (" << i->second.size() << ") subprograms: {";
			for (auto i_s = i->second.begin(); i_s != i->second.end(); ++i_s)
			{
				cerr << "Found overlap with subprogram ";
				if (i_s->second.name_here()) cerr << *i_s->second.name_here();
				else cerr << "0x" << std::dec << i_s->second.offset_here() << std::dec;
				cerr << endl;
			}
		}
		if (i->second.size() >= 1 && !one_seen) one_seen = i->second.begin()->second;
	}
	cerr << "nseen at " << std::hex << pc << ": " << std::dec << nseen << endl;
	if (unique && one_seen) return one_seen;
	return iterator_base::END;
}

/* FIXME: sysdep / check architecture first */
static
bool is_callee_save_register(int col)
{
	return col == DWARF_X86_64_RBX
		|| col == DWARF_X86_64_RBP
		|| col == DWARF_X86_64_R12
		|| col == DWARF_X86_64_R13
		|| col == DWARF_X86_64_R14
		|| col == DWARF_X86_64_R15;
}
// FIXME: put these elsewhere?
#ifndef NDEBUG
// used for sanity-checking
unsigned count_intervals(const frame_intervals_t& f)
{
	unsigned count = 0;
	for (auto i = f.begin(); i != f.end(); ++i, ++count);
	return count;
}
bool sanity_check_frame_element(const allocs::tool::frame_element& t)
{
	return true; // FIXME: meaningful sanity check
}
#endif

set< pair< boost::icl::discrete_interval<Dwarf_Addr>, frame_element > >
frame_element::cfi_elements_for(core::Fde fde,
	core::FrameSection& fs,
	subprogram_vaddr_interval_map_t const& subprograms)
{
	set< pair< boost::icl::discrete_interval<Dwarf_Addr>, frame_element > > out;

	using core::FrameSection;
	using core::Cie;
	using dwarf::encap::expr_instr;

	Dwarf_Addr fde_lopc = fde.get_low_pc();
	Dwarf_Addr fde_hipc = fde.get_low_pc() + fde.get_func_length();
	auto fde_interval = boost::icl::discrete_interval<Dwarf_Addr>::right_open(fde_lopc, fde_hipc);

	cerr << "Considering FDE beginning 0x" << std::hex << fde_lopc << std::dec << endl;

	/* Enumerate the overlapping subprograms. Warn if the count is not
	 * exactly 1. */
	iterator_df<subprogram_die> i_subp;
	if (!(i_subp = unique_subprogram_at(subprograms, fde_lopc)))
	{
		cerr << "FDE address 0x" << std::hex << fde_lopc << " does not belong"
			<< " to a unique subprogram" << endl;
	}
	/* Enumerate the locations of saved registers */
	const Cie& cie = *fde.find_cie();

	cerr << "Processing FDE for range " << std::hex << fde_lopc << "-"
		<< fde_hipc << std::dec << " (subprogram ";
	if (!i_subp) { cerr << "(unknown)"; }
	else
	{
		if (i_subp.name_here()) cerr << *i_subp.name_here();
		else cerr << "0x" << std::hex << i_subp.offset_here() << std::dec;
	}
	cerr << ")" << endl;

	/* decode the FDE */
	auto result = fde.decode();
	result.add_unfinished_row(fde.get_low_pc() + fde.get_func_length());

	// enumerate our columns
	set<int> all_columns;
	all_columns.insert(DW_FRAME_CFA_COL3);
	for (auto i_row = result.rows.begin(); i_row != result.rows.end(); ++i_row)
	{
		for (auto i_reg = i_row->second.begin(); i_reg != i_row->second.end(); ++i_reg)
		{
			all_columns.insert(i_reg->first);
		}
	}
	// visit them
	typedef std::function<void(int, optional< pair<int, FrameSection::register_def> >)>
	 visitor_function;
	int ra_rule_number = cie.get_return_address_register_rule();
	auto visit_columns = [all_columns, ra_rule_number](
		 visitor_function visit, 
		 optional<const set< pair<int, FrameSection::register_def> > &> opt_i_row
		) {

		auto get_column = [&opt_i_row](int col) {

			if (!opt_i_row) return optional< pair<int, FrameSection::register_def> >();
			else
			{
				map<int, FrameSection::register_def> m(opt_i_row->begin(), opt_i_row->end());
				auto found = m.find(col);
				return found != m.end() ? make_pair(found->first, found->second) : optional< pair<int, FrameSection::register_def> >();
			}
		};

		// always visit CFA column
		visit(DW_FRAME_CFA_COL3, get_column(DW_FRAME_CFA_COL3));

		// visit other columns that exist, except the ra rule for now
		for (auto i_col = all_columns.begin(); i_col != all_columns.end(); ++i_col)
		{
			if (*i_col != DW_FRAME_CFA_COL3 && *i_col != ra_rule_number)
			{
				visit(*i_col, get_column(*i_col));
			}
		}

		// finally, always visit the ra rule 
		visit(ra_rule_number, get_column(ra_rule_number));
	};

#ifndef NDEBUG
	auto sanity_check_post = [](const frame_intervals_t& f, unsigned previous_size) {
		unsigned count = count_intervals(f);
		if (count != f.size())
		{
			cerr << "Warning: count " << count << " != iterative size " << f.iterative_size() 
				<< " (previous size: " << previous_size << ")" 
				<< endl;
		}
		assert(count == f.iterative_size());
		/* Also sanity-check the member sets. */
		for (auto i = f.begin(); i != f.end(); ++i, ++count)
		{
			unsigned set_size = 0;
			for (auto i_s = i->second.begin(); i_s != i->second.end(); ++i_s, ++set_size)
			{

			}
			if (set_size != i->second.size())
			{
				cerr << "Warning: set iterative size " << set_size
					 << " != claimed size() " << i->second.size()
				<< endl;
			}
		}
	};
	#define SANITY_CHECK_PRE(f) /* do { unsigned count = count_intervals(f); \
		cerr << "Adding an interval (width " << (the_interval.upper() - the_interval.lower()) \
			<< " to a map of size " << f.size() << ", count " << count << endl; */ \
			do { for (auto i = f.begin(); i != f.end(); ++i) { \
				assert(sanity_check_frame_element(i->second)); \
			} } while (0)

	#define SANITY_CHECK_POST(f) /* sanity_check_post(f, count); } while (0) */
#else
	#define SANITY_CHECK_PRE(f) do { 
	#define SANITY_CHECK_POST(f)  } while (0)
#endif

	visitor_function row_column_visitor = [all_columns, ra_rule_number,
		fde_lopc, fde_hipc, &out]
		(int col, optional< pair<int, FrameSection::register_def> > found_col)  -> void {

		if (!found_col /*|| !is_callee_save_register(col)*/) {} // s << std::left << "u" << std::right;
		else
		{
			auto the_interval = boost::icl::discrete_interval<Dwarf_Addr>::right_open(
				fde_lopc,
				fde_hipc
			);
			int regnum; // used by saved-in-register cases
			shared_ptr<loc_expr> p_expr; // used by all non-trivial cases
			int saved_offset; // used by saved-at-offset-from-cfa, val-is-offset-from-cfa
			switch (found_col->second.k)
			{
				case FrameSection::register_def::INDETERMINATE:
				case FrameSection::register_def::UNDEFINED: 
					break;
				case FrameSection::register_def::REGISTER:
					// caller's register "col" is saved in callee register "regnum"
					regnum = found_col->second.register_plus_offset_r().first;
					goto saved_in_regnum;
				case FrameSection::register_def::SAME_VALUE:
					// "This register has not been modified from the previous frame.
					// (By convention, it is preserved by the callee, but the callee
					// has not modified it.)"
					// In other words, "whatever the register number they say I am,
					// that's what I am."
					regnum = col;
					goto saved_in_regnum;
				saved_in_regnum:
					assert(regnum <= 31);
					p_expr = make_shared<loc_expr>(loc_expr(
						{ (expr_instr) { .lr_atom = DW_OP_reg0 + regnum } }));
					goto saved_with_loc_expr;
				case FrameSection::register_def::SAVED_AT_OFFSET_FROM_CFA:
					// caller's register "col" is saved at "saved_offset" from CFA
					saved_offset = found_col->second.saved_at_offset_from_cfa_r();
					goto offset_from_cfa_cases;
				case FrameSection::register_def::VAL_IS_OFFSET_FROM_CFA:
					// caller's register "col" is not saved, but has the value computable
					// as "cfa + integer-value"
					saved_offset = found_col->second.val_is_offset_from_cfa_r();
					goto offset_from_cfa_cases;
				offset_from_cfa_cases:
					p_expr = make_shared<loc_expr>(loc_expr(
					 /* expr */ { (expr_instr) { .lr_atom = DW_OP_call_frame_cfa },
						  (expr_instr) { .lr_atom = DW_OP_consts, .lr_number = saved_offset },
						  (expr_instr) { .lr_atom = DW_OP_plus } }
					));
					if (found_col->second.k == FrameSection::register_def::VAL_IS_OFFSET_FROM_CFA)
					{ p_expr->push_back((expr_instr) { .lr_atom = DW_OP_stack_value }); }
					goto saved_with_loc_expr;
				case FrameSection::register_def::SAVED_AT_EXPR:
					// caller's register "col" is saved at location given by <expr>
					p_expr = make_shared<loc_expr>(loc_expr(
						found_col->second.saved_at_expr_r()
					));
					goto saved_with_loc_expr;
				case FrameSection::register_def::VAL_OF_EXPR:
					// caller's register "col" is not saved, but has the value computable
					// as "<expr>"
					p_expr = make_shared<loc_expr>(loc_expr(
						found_col->second.val_of_expr_r()
					));
					p_expr->push_back((expr_instr) { .lr_atom = DW_OP_stack_value });
					goto saved_with_loc_expr;
				saved_with_loc_expr: {
					/* The expr might have multiple DW_OP_piece-s, in the cases where
					 * we did not just create it ourselves. */
					auto pieces = p_expr->all_pieces();
					for (auto i_piece = pieces.begin();
						i_piece != pieces.end(); ++i_piece)
					{
						SANITY_CHECK_PRE(out);
						out.insert(make_pair(the_interval,
							frame_element(/* caller reg */ col, *i_piece, p_expr)));
						SANITY_CHECK_POST(out);
					}
				} break;
				default:
					break;
			}
		}
	};

	// process the row contents
	for (auto i_int = result.rows.begin(); i_int != result.rows.end(); ++i_int)
	{
		visit_columns(row_column_visitor, i_int->second);
	}

// FIXME: do we get the return address as a saved program counter? if not, how can we?
// f.s.v.o. return address, but we should leave that as an ABI thing rather than
// getting clever with expressions (we can't know the offset from call site to return target address)

	return out;
}

} // end namespace tool
} // end namespace allocs
