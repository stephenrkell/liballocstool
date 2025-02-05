#ifndef ALLOCSTOOL_FRAME_ELEMENT_HPP_
#define ALLOCSTOOL_FRAME_ELEMENT_HPP_

#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <string>
#include <cctype>
#include <cstdlib>
#include <memory>
#include <boost/icl/interval_map.hpp>
#include <dwarfpp/lib.hpp>
#include <dwarfpp/frame.hpp>

#include "stickyroot.hpp"

namespace allocs {
namespace tool {

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
using namespace dwarf::lib;
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

/* Since interval_map splits but doesn't consolidate, we may have a
 * collection of contiguous intervals with the same elements.
 * We want to process them all in one go. This is a bit like doing
 * equal_range() on a multimap. It can be used in a for loop,
 * at the rather nasty cost of updating the iterator in the loop body.
 * Note that the equal range is right-closed: [start, i_last_equal]. */
template <typename V>
typename boost::icl::interval_map<Dwarf_Addr, V>::iterator
find_equal_range_last(typename boost::icl::interval_map<Dwarf_Addr, V>::iterator start,
                      typename boost::icl::interval_map<Dwarf_Addr, V>::iterator end)
{
	if (start == end) return end;
	auto i_last_equal = start;
	Dwarf_Addr real_end = start->first.upper();
	while (++i_last_equal != end
		&& /* equality */   (i_last_equal->second == start->second ||
		 (/*cerr << "next not equal: " << std::hex << i_last_equal->first << std::dec << endl,*/ false))
		&& /* contiguity */ (i_last_equal->first.lower() == real_end ||
		 (/*cerr << "not contiguous with " << std::hex << real_end << ": " << i_last_equal->first << endl,*/ false))
	)
	{ real_end = i_last_equal->first.upper(); }
	--i_last_equal; // we incremented it once too far
	return i_last_equal; // note that the equal range is right-closed: [start, i_last_equal]
}

struct subprogram_key : public pair< pair<string, string>, string > // ordering for free
{
	subprogram_key(const string& subprogram_name, const string& sourcefile_name, 
		const string& comp_dir) : pair(make_pair(subprogram_name, sourcefile_name), comp_dir) {}
	string subprogram_name() const { return first.first; }
	string sourcefile_name() const { return first.second; }
	string comp_dir() const { return second; }
};

/* We gather subprograms by the ranges they cover
 * AND by their identity (key). */
typedef boost::icl::interval_map<
	Dwarf_Off,
	/* It's a set only so that we can detect and warn about overlaps... */
	std::set< pair< subprogram_key, iterator_df<subprogram_die> > >
> subprogram_vaddr_interval_map_t;

/* What's a frame element?
 * It's a piece of a frame. We have boiled away some of the DWARF features --
 * location lists / multiple vaddr ranges (each frame element only applies
 *   to a single range, as recorded in the interval map);
 * pieces (each frame element is a single piece).
 *
 * Just adding intervals to our map doesn't do this... does it?
 * How did our original frametypes work? We ignored pieces, but
 * clearly we did process location lists. I guess it's because we
 * extracted the ranges for each local... we would get a different
 * range for each location list element. Was that it? Did we even
 * get its intervals? Yes, we iterated over loclist elements directly. And
 * we still do this! We had:
 * singleton_set.insert(make_pair(i_dyn, *i_locexpr));
 * ...
 * subp_vaddr_intervals += make_pair(
 *    our_interval,
 *    singleton_set
 * );
 * ... our frame_element here is just a souped-up version of that pair.
 * It does *not* know the PC range to which it applies... that way,
 * the interval map can do its aggregating magic.
 *
 * However, it feels wrong not to have the construction logic here,
 * because it is tied up with the invariants of the class (e.g. effective_expr
 * contains no DW_OP_piece, etc). Perhaps we can define helper functions that
 * return a *set* of frame elements, given a DIE or a
 * 
 * We can also distinguish physical frame elements,
 *
 * What about inlined local vars? Our BFS treats them uniformly with top-level
 * non-inlined local vars. Every var has an enclosing scope-defining element,
 * even if it's just a lexical block
 *  */
struct frame_element
{
	iterator_df<with_dynamic_location_die> m_local;
	optional<Dwarf_Signed> m_caller_regnum;
	static inline Dwarf_Unsigned size_for_regnum(Dwarf_Unsigned regnum)
	{ /* FIXME: not accurate for all regs */ return sizeof (void*); }
	/* This gets the size not of the frame element, but of the logical
	 * program element e.g. the *whole* local var. This element might
	 * only be a piece of that. */
	optional<Dwarf_Unsigned> program_element_size_in_bytes() const
	{ return m_local ?
		(m_local->find_type()->calculate_byte_size()
			? optional<Dwarf_Unsigned>(*m_local->find_type()->calculate_byte_size())
			 : optional<Dwarf_Unsigned>())
		: optional<Dwarf_Unsigned>(size_for_regnum(*m_caller_regnum)); }
	// need another case: computed. and maybe another: implicit pointer.
	// and maybe also implicit_value but let's gamble we can rewrite that as a lit-style expression
	// and another: static masquerading as local: how does this emerge? it's a DW_TAG_variable
	//   so it is *both* a with_static_location and a with_dynamic_location... whether a
	//   location is really "static" or "dynamic" depends 

	// in all cases we have...
	//loc_expr effective_expr; // has been de-piece'd, selected from loc list, etc.
		// and rewritten-in-terms-of-CFA'd of course
	shared_ptr<loc_expr> p_effective_expr;
	loc_expr::piece effective_expr_piece;
	optional<Dwarf_Unsigned> piece_size_in_bytes() const
	{ return effective_expr_piece.size_in_bytes(); }

	optional<Dwarf_Signed> has_fixed_offset_from_frame_base() const;
	/*optional<Dwarf_Signed>*/ bool has_fixed_register() const
	{ return effective_expr_piece.op_count() == 1 && ( (effective_expr_piece.last_op().lr_atom >= DW_OP_reg0
		&& effective_expr_piece.last_op().lr_atom <= DW_OP_reg31) || effective_expr_piece.last_op().lr_atom == DW_OP_regx ); }
	/*optional<loc_expr>*/ bool has_value_function() const // strip DW_OP_stack_value at end? NO
	{ return effective_expr_piece.op_count() >= 1 && effective_expr_piece.last_op().lr_atom == DW_OP_stack_value; }
	                       // has_location implies has_value_function? just put deref at the end?

	/* DW_OP_implicit_pointer means "our value is a pointer into some other
	 * object, as given by this DIE reference (operand 1); we point
	 * this many bytes into that object (operand 2)". The other object
	 * can be either computed or stored, but if stored it is stored
	 * in a register not memory (otherwise the pointer could be explicit).
	 * Do we want to split those cases here?
	 * I suppose an implicit pointer could point to/within another implicit
	 * pointer... "within" would be odd, but "to" would be totally reasonable.
	 */
	//optional< pair<iterator_df<with_dynamic_location_die>, Dwarf_Unsigned > >
	bool
	has_implicit_pointer_value() const
	{ return effective_expr_piece.op_count() >= 1 && (
#ifdef DW_OP_implicit_pointer
	effective_expr_piece.last_op().lr_atom == DW_OP_implicit_pointer ||
#endif
	effective_expr_piece.last_op().lr_atom == DW_OP_GNU_implicit_pointer
	   ); }

	//optional< vector<unsigned char> >
	bool
	has_implicit_literal_value() const
	{ return effective_expr_piece.op_count() >= 1 && (
#ifdef DW_OP_implicit_value
	effective_expr_piece.last_op().lr_atom == DW_OP_implicit_value
	/* FIXME: how does libdwarf parse these? What about libdw? From my dwarf.h I see
    DW_OP_implicit_value = 0x9e, // DW_FORM_block follows opcode.
	   ... so it might not be safe to assume the last expr_instr is the DW_OP_implicit_value */
#else
	false
#endif
	  ); }

	bool has_implicit_value() const
	{ return has_implicit_pointer_value() || has_implicit_literal_value(); }

	/* A single location expression need not denote a fixed offset
	 * or fixed register. When they don't, it makes life harder
	 * for us. How often does this happen? I think for now
	 * we should give up on cases where this is true... or
	 * ACTUALLY treat them like computed values, where internally
	 * we compute the location and then deref it. */
	/*optional<loc_expr>*/ bool has_location() const          // does not exclude fixed offset/register
	{ return !has_value_function() && !has_implicit_value(); }
	/*optional<loc_expr>*/ bool has_varying_location() const // does exclude
	{ return has_location() && !has_fixed_offset_from_frame_base() && !has_fixed_register(); }

	bool                   location_depends_on_register() const;
	bool                   is_static_masquerading_as_local() const
	{ return m_local && has_location() && /* HACK: look for DW_OP_addr at the start of an expr */
		effective_expr_piece.op_count() > 0 && effective_expr_piece.first->lr_atom == DW_OP_addr; }

	optional<Dwarf_Signed> is_saved_register() const
	{ return (m_caller_regnum && *m_caller_regnum != DW_FRAME_CFA_COL3)
	  ? m_caller_regnum : optional<Dwarf_Signed>(); }
	bool is_current_cfa() const
	{ return (m_caller_regnum && *m_caller_regnum == DW_FRAME_CFA_COL3); }
	iterator_df<with_dynamic_location_die> is_local() const { return m_local; }

	/* When constructing, we always need to have a PC range:
	 * set effective expression => we need to know the element's PC range (or just lopc) */

private:
	frame_element(Dwarf_Unsigned reg, loc_expr::piece const& piece, shared_ptr<loc_expr> whole_expr)
	 : m_local(iterator_base::END), m_caller_regnum(reg), effective_expr_piece(piece),
	   p_effective_expr(whole_expr) {}
	frame_element(iterator_df<with_dynamic_location_die> d, loc_expr::piece const& piece, shared_ptr<loc_expr> whole_expr)
	 : m_local(std::move(d)), m_caller_regnum(), effective_expr_piece(piece),
	   p_effective_expr(whole_expr)
	  { assert(m_local); }
public:
	static
	set< pair< boost::icl::discrete_interval<Dwarf_Addr>, frame_element > >
	local_elements_for(iterator_df<with_dynamic_location_die> d, iterator_df<subprogram_die> i_subp,
		sticky_root_die& r);
	static
	set< pair< boost::icl::discrete_interval<Dwarf_Addr>, frame_element > >
	cfi_elements_for(core::Fde fde,
		core::FrameSection& fs,
		subprogram_vaddr_interval_map_t const& subprograms);
};

typedef boost::icl::interval_map<
		Dwarf_Off /* interval base type */,
		set<frame_element>
	> frame_intervals_t;

bool operator<(const frame_element& x,
		       const frame_element& y);

bool operator==(const frame_element& x,
		       const frame_element& y);

}
}
#endif
