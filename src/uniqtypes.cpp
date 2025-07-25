#include "uniqtype-defs.h" /* for UNIQTYPE_DECL which we stringify -- include first to avoid
                            * conflicting C++-linkage decls coming later from <c*> */
#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <string>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cstddef>
#include <memory>
#include <cmath>
#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/icl/interval_map.hpp>
#include <srk31/algorithm.hpp>
#include <srk31/ordinal.hpp>
#include <cxxgen/tokens.hpp>
#include <cxxgen/cxx_compiler.hpp>
#include <dwarfpp/lib.hpp>
#include <fileno.hpp>
#include <err.h> /* for warnx */

#include "uniqtypes.hpp"
#include "bitops.h"

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
using std::pair;
using std::make_pair;
using namespace dwarf;
//using boost::filesystem::path;
using dwarf::core::iterator_base;
using dwarf::core::iterator_df;
using dwarf::core::iterator_sibs;
using dwarf::core::type_die;
using dwarf::core::subprogram_die;
using dwarf::core::type_describing_subprogram_die;
using dwarf::core::program_element_die;
using dwarf::core::compile_unit_die;
using dwarf::core::member_die;
using dwarf::core::with_data_members_die;
using dwarf::core::variable_die;
using dwarf::core::with_dynamic_location_die;
using dwarf::core::address_holding_type_die;
using dwarf::core::base_type_die;
using dwarf::core::enumeration_type_die;
using dwarf::core::subrange_type_die;
using dwarf::core::array_type_die;
using dwarf::core::string_type_die;
using dwarf::core::type_chain_die;
using dwarf::core::subroutine_type_die;
using dwarf::core::unspecified_type_die;
using dwarf::core::formal_parameter_die;
using dwarf::spec::opt;

using dwarf::lib::Dwarf_Off;
using dwarf::lib::Dwarf_Unsigned;
using dwarf::lib::Dwarf_Signed;

using dwarf::tool::abstract_c_compiler;

using boost::regex;
using boost::regex_match;
using boost::smatch;
using boost::regex_constants::egrep;
using boost::match_default;
using boost::format_all;

namespace allocs
{
namespace tool
{
/* Forward declaration of a local helper function */
static void write_uniqtype_open_generic(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    const string& pos_maxoff_str,
	bool use_section_group = true,
	bool make_weak_definition = false
	);

/* This function just deals with non-concretes, or base types needing aliases;
 * it delegates other cases to add_concrete_type_if_absent. */
pair<bool, codeful_name> add_type_if_absent(iterator_df<type_die> t, master_relation_t& r)
{
	auto concrete_t = t ? t->get_concrete_type() : iterator_df<type_die>();
	if (t && t != concrete_t) // this handles typedef_die, mostly...
	{
		// add the concrete, but alias it as the non-concrete
		auto concrete_t = t->get_concrete_type();
		auto ret = add_concrete_type_if_absent(concrete_t, r);
		// add the alias, if we have a name *and* it is different from
		// whatever the concrete's name will be if we generate it
		opt<string> maybe_t_name = t.name_here() ? codeful_name(t).second : opt<string>();
		string concrete_t_name = codeful_name(concrete_t).second;
		if (maybe_t_name && *maybe_t_name != concrete_t_name)
		{
			// add the non-concrete alias
			add_alias_if_absent(/* the alias */ *type_die_get_name(t),
				/* the thing aliased */ concrete_t, r);
		}
		return ret;
	}
	else if (t.is_a<base_type_die>())
	{
		/* Base types are a bit like non-concretes. */
		// add the concrete
		auto concrete_t = t->get_concrete_type();
		auto ret = add_concrete_type_if_absent(concrete_t, r);
		// add the alias, if we have a name
		if (t.name_here() && *t.name_here() != "__uninterpreted_byte"
			&& !t.as_a<base_type_die>()->is_bitfield_type())
		{
			add_alias_if_absent(*type_die_get_name(t), concrete_t, r);
			/* HACK: for good measure, also ensure that we add the 
			 * canonical C name, if the name we have is in some equivalence class. */
			const char **c_equiv_class = abstract_c_compiler::get_equivalence_class_ptr(
				type_die_get_name(t)->c_str());
			if (c_equiv_class)
			{
				add_alias_if_absent(c_equiv_class[0], concrete_t, r);
			}
		}
		return ret;
	}
	else return add_concrete_type_if_absent(t, r);
}
void add_alias_if_absent(const string& s, iterator_df<type_die> concrete_t, master_relation_t& r)
{
	// HACK: don't alias void (we can't use iterators-to-void-type as indexes)
	if (!concrete_t) return;
	
	/* HACK: since in C, "struct X" and "X" are distinct, but we don't distinguish them, 
	 * we also need to ignore this kind of alias here. Be careful about base types though: 
	 * we *do* need their actual-name aliases. */
	if (!concrete_t.is_a<base_type_die>()
		&& concrete_t.name_here()
		&& s == *type_die_get_name(concrete_t)) return;
	
	r.aliases[codeful_name(concrete_t)].insert(s);
}
pair<bool, codeful_name> add_concrete_type_if_absent(iterator_df<type_die> t, master_relation_t& r)
{
	// we might get called on to add void
	if (t == iterator_base::END)
	{
		return make_pair(false, make_pair("", ""));
	}

	assert(t == t->get_concrete_type());

	
// 	/* If it's a base type, we might not have a decl_file, */
// 	if (!t->get_decl_file() || *t->get_decl_file() == 0)
// 	{
// 		if (t.tag_here() != DW_TAG_base_type
// 		 && t.tag_here() != DW_TAG_pointer_type
// 		 && t.tag_here() != DW_TAG_reference_type
// 		 && t.tag_here() != DW_TAG_rvalue_reference_type
// 		 && t.tag_here() != DW_TAG_array_type
// 		 && t.tag_here() != DW_TAG_subroutine_type)
// 		{
// 			cerr << "Warning: skipping non-base non-pointer non-array non-subroutine type described by " << *t //
// 			//if (t.name_here()) cerr << t.name_here();
// 			//else cerr << "(unknown, offset: " << std::hex << t.offset_here() << std::dec << ")";
// 			/*cerr */ << " because no file is recorded for its definition." << endl;
// 			return make_pair(false, make_pair("", ""));
// 		}
// 		// else it's a base type, so we go with the blank type
// 		// FIXME: should canonicalise base types here
// 		// (to the same as the ikind/fkinds come out from Cil.Pretty)
// 	}

	codeful_name n(t);
	
	smatch m;
	bool already_present = r.find(n) != r.end();
	if (already_present
		&& t.tag_here() != DW_TAG_base_type
		&& !regex_match(n.second, m, regex(".*__(PTR|REF|FUN|RR|ARR[0-9]+)_.*")))
	{
		// cerr << "warning: non-base non-pointer non-array non-function type named " << n.second << " already exists!" << endl;
	}
	r[n] = t;
	return make_pair(!already_present, n);
}

pair<bool, codeful_name> transitively_add_type(iterator_df<type_die> toplevel_t, master_relation_t& r)
{
	pair<bool, codeful_name> result;
	
	walk_type(toplevel_t, iterator_base::END, 
		[&r, &result, toplevel_t](iterator_df<type_die> t, iterator_df<program_element_die> reason) -> bool {
		/* NOTE: we will get called for every type, including void. 
		 * Our job is to decide whether we need to add_type_if_absent, 
		 * and whether we need to recurse. */

		cerr << "Walking: " << t << endl;
		
		if (reason.is_a<member_die>())
		{
			auto memb = reason.as_a<member_die>();
			if (memb->get_declaration() && *memb->get_declaration()
				 && memb->get_external() && *memb->get_external())
			{
				// static member vars don't get added nor recursed on
				return false;
			}
			
			assert(memb->get_type() != iterator_base::END);
			if (memb->get_type()->get_concrete_type() == t.parent().as_a<type_die>()) 
			{
				cout << "Found directly recursive data type: "
					<< t
					<< " contains member "
					<< memb
					<< " of type "
					<< memb->get_type()->get_concrete_type()
					<< " which equals " 
					<< t.parent()
					<< endl;
				assert(false);
			}
		}
		
		auto p = add_type_if_absent(t, r);
		if (!p.first) return false; // we've already added it; stop now
		
		// the result of the calling function is the toplevel case of the walk
		if (t == toplevel_t) result = p;
		
		return true; // keep going
	});
	
	return make_pair(true, result.second);
}

void make_exhaustive_master_relation(master_relation_t& rel, 
	dwarf::core::iterator_df<> begin, 
	dwarf::core::iterator_df<> end)
{
	lib::Dwarf_Off previous_offset = 0UL;
	bool done_some_output = false;
	for (iterator_df<> i = begin; i != end; ++i)
	{
		assert(i.offset_here() >= previous_offset); // == for initial case, > afterwards
		if (i.is_a<type_die>())
		{
			// HACK: don't add subrange types -- should really restrict to the array case (FIXME)
			if (i.is_a<subrange_type_die>()) continue;
			if (isatty(fileno(std::cerr)))
			{
				if (done_some_output) std::cerr << "\r";
				std::cerr << "Master relation: adding DIE at 0x" << std::hex << i.offset_here() << std::dec;
				done_some_output = true;
			}
			// add it to the relation
			opt<string> opt_name = !i.is_a<subprogram_die>() ? i.name_here() : opt<string>(); // for debugging
			if (opt_name)
			{
				string name = *opt_name;
				assert(name != "");
			}
			add_type_if_absent(i.as_a<type_die>(), rel);
		}
		else if (i.is_a<member_die>())
		{
			/* If we have any of the bit attributes, we might induce another type.
			 * So add it. */
			auto memb = i.as_a<member_die>();
			if (memb->get_bit_size() || memb->get_bit_offset() || memb->get_data_bit_offset())
			{
				add_type_if_absent(memb->find_or_create_type_handling_bitfields(), rel);
			}
		}
		previous_offset = i.offset_here();
	}
	/* As a bit of a HACK, we make an exception if the non-concrete
	 * has a name and the concrete doesn't. For example, with
	 *
	 * typedef struct {
	 * ...
	 * } blah;
	 *
	 * we want 'blah' to denote the struct type. We still generate
	 * an alias with the autogenerated 'anonymous' name, but only
	 * an alias. We do this as a rewrite after the relation is done;
	 * otherwise we have no way to be sure that the alias is unique.
	 */
	map< codeful_name, iterator_df<type_die> > to_re_add;
	master_relation_t::iterator i_rel = rel.begin();
	while (i_rel != rel.end())
	{
#if 0
		/* Are with a with-data-members DIE (i.e. "normally" we'd have a name),
		 * with no source-level name,
		 * with a unique alias that does have a source-level name? */
		if (i_rel->second && i_rel->second.is_a<with_data_members_die>()
			&& !i_rel->second.name_here()
			&& rel.aliases[i_rel->first].size() == 1)
		{
			master_relation_t::value_type v = *i_rel;
			codeful_name initial_key = v.first;
			string unique_alias = *rel.aliases[initial_key].begin();
			codeful_name actual_key = make_pair(/* code */ initial_key.first,
				/* uniqtype name */ unique_alias);
			std::cerr << "Swapping '" << initial_key.second
				<<  "' and '" << unique_alias << "'" << std::endl;
			i_rel = rel.erase(i_rel);
			/* Also erase the alias, now that there is a bona-fide
			 * type of that name.
			 * PROBLEM: what if there are others aliases with the same
			 * name and code? Should we erase those too?
			 * AHA, but wasn't this already a problem? We shouldn't have
			 * same-name-same-code aliases under two different keys,
			 * and we weed them out later anyway (below).
			 */
			rel.aliases[actual_key].insert(initial_key.second);
			to_re_add.insert(make_pair(actual_key, v.second));
		}
		else
#endif
		++i_rel;
	}
	for (auto i_re_add = to_re_add.begin(); i_re_add != to_re_add.end(); ++i_re_add)
	{
		rel.insert(*i_re_add);
	}
	/* Finally we need to sanity-check aliases. We were seeing cases like this:
	
   168  extern struct uniqtype __uniqtype_01082207__build_glibc_S9d2JN_glibc_2_27_iconv____wcsmbs_bits_types___mbstate_t_h_13;
   169  extern struct uniqtype __uniqtype_01082207___mbstate_t __attribute__((weak));
   171  extern struct uniqtype __uniqtype_01082207__build_glibc_S9d2JN_glibc_2_27_misc____wcsmbs_bits_types___mbstate_t_h_13;
   172  extern struct uniqtype __uniqtype_01082207___mbstate_t __attribute__((weak));
   174  extern struct uniqtype __uniqtype_01082207__build_glibc_S9d2JN_glibc_2_27_posix____wcsmbs_bits_types___mbstate_t_h_13;
   175  extern struct uniqtype __uniqtype_01082207___mbstate_t __attribute__((weak));

	 * ... where each of the apparently distinct include paths
	 * (really symlinked) defines a typedef for an anonymous struct
	 * that of course has an identical definition, so an identical typecode,
	 * so an identical codeful alias symbol.
	 * But because each alias symbol points to a *different* anonymous struct,
	 * our code thinks they are each unique aliases
	 * to a coincidentally aliased.
	 *
	 * We should do something to guard against coincidental same-name-same-typecode cases.
	 * Otherwise we will emit many aliases under the same symbol,
	 * but try to point them at different symbols (compile error)
	 * and likely try to define the aliases in different sections
	 * (another compiler error).
	 *
	 * So we do a pass to weed out same-name, same-typecode aliases.
	 * with different targets.
	 */
	i_rel = rel.begin();
	map<string, master_relation_t::iterator > alias_symbols_seen; // <alias symbol, target symbol>
	while (i_rel != rel.end())
	{
		string target_symbol = mangle_typename(i_rel->first);
		auto& aliases = rel.aliases[i_rel->first];
		auto i_alias = aliases.begin();
		while (i_alias != aliases.end())
		{
			string codeful_alias_symbol = mangle_typename(make_pair(i_rel->first.first,
				*i_alias));
			auto pos_and_really_inserted = alias_symbols_seen.insert(
				make_pair(codeful_alias_symbol, i_rel)
			);
			if (!pos_and_really_inserted.second)
			{
				/* Was not inserted -> an entry for codeful_alias_symbol
				 * exists already. */
				master_relation_t::iterator earlier_seen_pos
				 = pos_and_really_inserted.first->second;
				string earlier_alias_target = mangle_typename(earlier_seen_pos->first);
				cerr << "Dropping suspiciously name-and-code-duplicate alias: "
					<< codeful_alias_symbol
					<< " (target here: "
					<< target_symbol
					<< "; first seen as alias of: "
					<< earlier_alias_target
					<< ")" << endl;
				/* If the targets are the same, then
				 * both aliases are at the same i_rel.
				 * But duplicates are not possible because
				 * aliases are stored in a set. */
				if (mangle_typename(pos_and_really_inserted.first->second->first)
				   == target_symbol)
				{
					// we're going to fail the assertion, but print out
					// something useful first.
					cerr << "Strange: found name- and target-identical aliases in master relation at "
						<< &*earlier_seen_pos
						<< " (key: <" << earlier_seen_pos->first.first << ", " << earlier_seen_pos->first.second << ">)"
						<< " and "
						<< &*i_rel
						<< " (key <" << i_rel->first.first << ", " << i_rel->first.second << ">)"
						<< std::endl;
					// We used to get this when we had a typedef 'long_int' as well as
					// base type 'long int', and in our old mangling they both mangled
					// to the same. It shouldn't happen any more
				}
				assert(mangle_typename(pos_and_really_inserted.first->second->first)
				 != target_symbol);
				i_alias = aliases.erase(i_alias);
			} else ++i_alias;
		}
		++i_rel;
	}
}
static void set_symbol_length(std::ostream& out, const string& mangled_name, unsigned length)
{
	out << "__asm__(\".size " << mangled_name << ", " << length
		<< "\");" << endl;
}
string ensure_contained_length(const string& mangled_name, unsigned contained_length)
{
	ostringstream s;
	set_symbol_length(s, mangled_name,
		offsetof(uniqtype, related) + contained_length * sizeof (uniqtype_rel_info));
	return s.str();
}
static string attributes_for_uniqtype(const string& mangled_name, bool is_weak = false, bool include_section = true)
{
	std::ostringstream s;
	bool need_comma = false;
	bool need_termination = false;
	if (is_weak || include_section)
	{
		s << " __attribute__((";
		need_termination = true;
	}
	if (include_section)
	{
		if (need_comma) s << ",";
		s << "section (\".data." << mangled_name
			<< ", \\\"awG\\\", @progbits, " << mangled_name << ", comdat#\")";
		need_comma = true;
	}
	if (is_weak)
	{
		if (need_comma) s << ",";
		s << "weak";
		need_comma = true;
	}
	if (need_termination) s << ")) ";
	return s.str();
}
void emit_weak_alias_idem(std::ostream& out, const string& alias_name, const string& target_name, bool emit_section /* = true */)
{
	// workaround for gcc bug 90470: don't emit the same alias twice
	static set<string> emitted_previously;
	if (emitted_previously.find(alias_name) != emitted_previously.end()) return;
	/* It's a bug if the target name is the same as the alias. */
	assert(alias_name != target_name);
	emitted_previously.insert(alias_name);
	out << "extern struct uniqtype " << alias_name
		<< " __attribute__((weak,alias(\"" << target_name << "\")";
	if (emit_section)
	{
		out << ",section(\".data." << target_name
			/* To satisfy gcc's "section of alias `...' must match section of its target",
			 * we rather we even have to match the escape-hatch cruft (although it gets
			 * discarded after gcc has done the check). */
			<< ", \\\"awG\\\", @progbits, " << target_name << ", comdat#"
			<< "\")";
	}
	out <<"));"
		<< endl;

	/* Make the alias symbol name short to be able to retrieve the original 
	 * symbol name with dladdr.
	 * Zero has a special meaning so use 1 instead. */
	set_symbol_length(out, alias_name, 1);
}
void emit_extern_declaration(std::ostream& out,
	const codeful_name& name_pair,
	bool force_weak)
{
	out << "extern struct uniqtype " << mangle_typename(name_pair);
	// incompletes are weak-ref'd, except "void" which is specal
	// FIXME: probably it should have summary code 0
	if (force_weak || (name_pair.first == "" && name_pair.second != "void"))
	{
		out << " __attribute__((weak))";
	}
	out << ";" << endl;
}
const char *pervasives_raw_names[] = { "void", "__EXISTS1___PTR__1", "__uninterpreted_byte" };

void write_master_relation(master_relation_t& r, 
	std::ostream& out, std::ostream& err,
	std::set< std::string >& names_emitted,
	std::map< std::string, std::set< dwarf::core::iterator_df<dwarf::core::type_die> > >& types_by_name,
	bool emit_codeless_aliases,
	bool emit_subobject_names /* = false */)
{
	std::map< std::string, std::set< pair<string, string> > > name_pairs_by_name;
	
	/* Some types are too obscure to be considered for the codeless
	 * alias thing. Specifically, this is bitfields: if we have a 
	 * bitfield type called "int", it should not prevent us choosing
	 * a generic alias "int". */
	std::map< std::string, std::set< string > > codeless_alias_blacklist;
	
	/* Note the very nasty hack with __attribute__((section (...))):
	 * we embed a '#' into the section string, after adding our own
	 * assembler-level flags and attributes. This causes the compiler-
	 * -generated flags and attributes to be ignored, because the '#'
	 * comments them out. Without this trick, there is no way of supplying
	 * our own section flags and attributes to override the compiler.
	 * FIXME: this works with gcc-generated assembly but not clang's.
	 * Borrow glibc's somewhat-portable way of doing this, if that fixes things.
	 * FIXME: fix the same thing elsewhere, too. */
	// always declare the pervasives, at least, with weak attribute
	for (const char **p_n = &pervasives_raw_names[0];
		p_n != pervasives_raw_names + (sizeof pervasives_raw_names / sizeof pervasives_raw_names[0]);
		++p_n)
	{
		const char *n = *p_n;
		// pervasives should not be weak-ref'd. If we need them, pull them in...
		// this is so that __PTR_TO_void will correctly pull in a reference to void
		// perhaps from roottypes.a (if it's generated by usedtypes)
		// i.e. because weak refs don't pull stuff from archives, but we want the
		// pull-on-demand semantics of archives
		// FIXME: probably this shouldn't be limited to void... anything else that
		// has no summary code? Maybe we should give void a summary of 00000000?
		out << "extern struct uniqtype " << mangle_typename(make_pair(string(""), string(n)))
			<< /*" __attribute__((weak))"*/";" << endl;
	}
	
	/* The complement relation among signed and unsigned integer types. */
	map<unsigned, map<bool, set< master_relation_t::value_type > > > integer_base_types_by_size_and_signedness;
	auto needs_complement = [](iterator_df<base_type_die> base_t) {
		return (base_t->get_encoding() == DW_ATE_signed
			 || base_t->get_encoding() == DW_ATE_unsigned)
			 && base_t->bit_size_and_offset().second == 0; 
			 /* HACK: only complement zero-off cases for now, since we don't track the 
			  * bit offset in the big _by_size_and_signedness map. */
	};
	auto avoid_aliasing_as = [](const string& alias, iterator_df<type_die> t) {
		if (!t.is_a<base_type_die>()) return false;
		auto base_t = t.as_a<base_type_die>();
		/* Funky bitfield types are 
		 * too obscure to be considered for codeless aliasing. */
		return (base_t->bit_size_and_offset().second != 0
			|| base_t->bit_size_and_offset().first != 8 * (*base_t->calculate_byte_size()));
	};
	/* Emit forward declarations, building the complement relation as we go.
	 * We only forward-declare things in the relation; we don't traverse
	 * dependencies, because the relation is assumed to be transitively closed.
	 * We already hack around one issue here (below): signedness complements that
	 * are not in the relation may nevertheless be depended on. */
	set<string> names_previously_emitted;
	for (auto i_pair = r.begin(); i_pair != r.end(); ++i_pair)
	{
		auto name = i_pair->first;
		string s = mangle_typename(name);
		bool not_previously_emitted = names_emitted.insert(s).second;
		if (!not_previously_emitted)
		{
			names_previously_emitted.insert(s);
			// don't skip the rest; complement stuff to do, and harmless to forward-decl it again
		}
		iterator_df<type_die> t = i_pair->second;
		if (t && t != t->get_concrete_type())
		{
			cerr << "Warning: master relation contained non-concrete: " << t << endl;
		}
		types_by_name[name.second].insert(t);
		if (name.first != "") name_pairs_by_name[name.second].insert(name);
		if (t.is_a<base_type_die>())
		{
			/* Are we an integer? */
			auto base_t = t.as_a<base_type_die>();
			if (needs_complement(base_t))
			{
				unsigned bit_size = base_t->bit_size_and_offset().first;
				bool signedness = (base_t->get_encoding() == DW_ATE_signed);

				// HACK: for now, skip weird cases with bit offset non-zero
				if (base_t->bit_size_and_offset().second == 0)
				{
					integer_base_types_by_size_and_signedness[bit_size][signedness].insert(*i_pair);
				}
			}
			/* The 'avoid_aliasing_as' thing handles bitfield types that should not be
			 * aliased codelessly. */
			if (avoid_aliasing_as(name.second, t))
			{
				codeless_alias_blacklist[name.second].insert(name.first);
			}
		}
		/* We have *almost* no need to declare aliases, because all the symnames
		 * that we emit shoud be canonical. But they're not, thanks to our
		 * hack for anonymous structs with a unique typedef. Because the
		 * anonymous struct's name is the canonical one, it gets emitted
		 * in various reference contexts, but the one we forward-declared
		 * above is the non-canonical (typedef) one. So we need that 'anonymous'
		 * alias declared. We just declare all aliases. */
		emit_extern_declaration(out, i_pair->first, /* force_weak */ false);
		auto& aliases = r.aliases[i_pair->first];
		for (auto i_alias = aliases.begin(); i_alias != aliases.end(); ++i_alias)
		{
			emit_extern_declaration(out, codeful_name(i_pair->first.first, *i_alias),
				/* force_weak */ true);
		}
	}
	/* Declare any signedness-complement base types that we didn't see. 
	 * We will emit these specially. */
	set< iterator_df<base_type_die> > synthesise_complements;
	for (auto i_size = integer_base_types_by_size_and_signedness.begin(); 
		i_size != integer_base_types_by_size_and_signedness.end(); ++i_size)
	{
		auto& by_signedness = i_size->second;
		
		/* We should never have nominally-distinct, definitionally-equivalent 
		 * base types. Different names should be aliases, only. */
		assert(by_signedness[false].size() <= 1);
		assert(by_signedness[true].size() <= 1);
		iterator_df<base_type_die> have_unsigned = iterator_base::END;
		if (by_signedness[false].size() == 1) have_unsigned = by_signedness[false].begin()->second.as_a<base_type_die>();
		iterator_df<base_type_die> have_signed = iterator_base::END;
		if (by_signedness[true].size() == 1) have_signed = by_signedness[true].begin()->second.as_a<base_type_die>();
		
		// if we don't have either, how did we get here?
		assert(have_unsigned || have_signed);
		
		if (!have_unsigned || !have_signed) // the "count == 1" case
		{
			// we have to synthesise the other-signedness version
			cerr << "We have " 
				<< (have_signed ? have_signed : have_unsigned)
				<< " but not its complement, so will synthesise it." << endl;
				
			synthesise_complements.insert(have_signed ? have_signed : have_unsigned);
		}
		// else the "count == 2" case: no need to synthesise
	}
	// declare any synthetic complements
	for (auto i_need_comp = synthesise_complements.begin(); 
		i_need_comp != synthesise_complements.end(); 
		++i_need_comp)
	{
		// compute and print complement name
		auto k = make_pair(
			summary_code_to_string(
				signedness_complement_type_summary_code(
					*i_need_comp
				)
			),
			name_for_complement_base_type(*i_need_comp)
		);
		string s = mangle_typename(k);

		out << "extern struct uniqtype " << s << " __attribute__((weak)); "
			<< "/* synthetic signedness complement of " << (*i_need_comp)->get_canonical_name()
			<< " */" << endl;
	}

	/* Output the canonical definitions. */
	for (auto i_vert = r.begin(); i_vert != r.end(); ++i_vert)
	{
		if (i_vert->first.second == string("void"))
		{
			if (i_vert->second) err << "Warning: skipping explicitly declared void type from CU "
				<< *i_vert->second.enclosing_cu().name_here()
				<< endl;
			continue;
		}
		string mangled_name = mangle_typename(i_vert->first);
		if (names_previously_emitted.find(mangled_name) != names_previously_emitted.end())
		{
			// we think we have done this one already, probably as an ARR0
			out << "\n/* We should have previously output a definition of uniqtype for \""
				<< i_vert->first.second 
				<< "\" with summary code " << i_vert->first.first << " */\n";
			continue;
		}
		auto opt_sz = i_vert->second->calculate_byte_size();
		
		out << "\n/* uniqtype for \"" << i_vert->first.second 
			<< "\" with summary code " << i_vert->first.first << " */\n";
		std::vector< iterator_base > real_members;
		std::vector< Dwarf_Unsigned > real_member_offsets;
		std::vector< iterator_base > fp_types;
		if (i_vert->second.is_a<type_describing_subprogram_die>())
		{
			auto fps = i_vert->second.children().subseq_of<formal_parameter_die>();
			for (auto i_edge = fps.first; i_edge != fps.second; ++i_edge)
			{
				fp_types.push_back(i_edge->find_type()); 
			}
		}
		else
		{
			auto members = i_vert->second.children().subseq_of<member_die>();
			for (auto i_edge = members.first; i_edge != members.second; ++i_edge)
			{
				/* if we don't have a byte offset, skip it ( -- it's a static var?) */
				opt<Dwarf_Unsigned> opt_offset = i_edge->byte_offset_in_enclosing_type(
					true /* assume packed -- needed for synthetic types' members */);
				if (!opt_offset)
				{
					err << "Warning: member " << i_edge.summary()
						<< " has no byte offset, so skipping" << std::endl;
					continue;
				}
				else
				{ 
					real_members.push_back(i_edge); 
					real_member_offsets.push_back(*opt_offset);
				}
			}
		}
		unsigned members_count = real_members.size();
		
		/* Our last chance to skip things we don't want to emit. 
		 * NOTE that for incompletes, we distinguish "flexible", "opaque" and "undefined" types
		 * (FIXME: actually use this terminology consistently).
		 * 
		 * "flexible" means it has some defined members, but no length; pos_maxoff will be -1. 
		 * "opaque" is things like functions which deliberately have no length nor contents;
		        pos_maxoff will be 0.
		 * "undefined" is structs that are declared but not defined. *Usually* the intention
		 * here is the same as for "opaque"... HMM.
		 */
		if (i_vert->second.is_a<with_data_members_die>() && real_members.size() == 0 && !opt_sz)
		{
			// we have an incomplete type -- skip it!
			err << "Warning: with-data-members type " 
				<< i_vert->first.second
				<< " is incomplete, skipping." << endl;
			out << "/* skipped -- incomplete */" << endl;
			continue;
		}
		
		/* We might not be incomplete, but be dependent on an incomplete somehow
		 * (e.g. points-to). We should emit something, because we might never see a
		 * definition for the thing we depend on, but the depending-on type has
		 * an independent existence. However, we should emit something weak, such
		 * that it will be replaced by any strong definition. Since we don't have a
		 * code, that strong definition will also be codeless. So emitting this
		 * weak definition should not prevent a codeless alias from appearing...
		 * i.e. it should not be considered to create ambiguity (ambiguity being
		 * what normally prevents the generation of codeless aliases). In other
		 * words, we should not add something to name_pairs_by_name. */
		bool dependent_on_incomplete = (i_vert->first.first == "");
		if (dependent_on_incomplete)
		{
			cout << "/* Depends on something incomplete, so definition should be weak. */"
				<< std::endl;
		}
		else
		{
			write_uniqtype_section_decl(out, mangled_name);
		}

		/* We can also be *variable-length*. In this case we output a pos_maxoff of -1
		 * i.e. maximum-unsigned-value. */
		if (emit_subobject_names)
		{
			out << "const char *" << mangled_name << "_subobj_names[] ";
			out << attributes_for_uniqtype(mangled_name, /* weak */ true,
				/* include section */ !dependent_on_incomplete);
			if (i_vert->second.is_a<with_data_members_die>())
			{
				out << " = { ";
				unsigned num = 0;
				for (auto i_i_edge = real_members.begin(); i_i_edge != real_members.end(); ++i_i_edge, ++num)
				{
					auto i_edge = i_i_edge->as_a<member_die>();

					if (i_edge.name_here())
					{
						string name = *i_edge.name_here();
						out << "\"" << name << "\"";
					}
					else
					{
						/* FIXME: do something nicer */
						out << "\"_" << num << "\"";
					}
					/* always output a comma */
					out << ", ";
				}
				out << "(void*)0 };\n";
			}
			else
			{
				out << "= { (void*)0 };\n";
			}
		}
		
		unsigned contained_length = 1;
		if (i_vert->second.is_a<array_type_die>())
		{
			unsigned array_len;
			auto opt_array_len = i_vert->second.as_a<array_type_die>()->element_count();
			if (opt_array_len) array_len = *opt_array_len;
			else array_len = 0;
			if (array_len > 0)
			{
				write_uniqtype_open_array(out,
					mangled_name,
					i_vert->first.second,
					(opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0)) /* pos_maxoff */,
					array_len
				);
			}
			else
			{
				/* FIXME: we should really distinguish the other cases of zero/0-length
				 * arrays. For now, just assume that the memory-bounds flex treatment
				 * is appropriate.*/
				write_uniqtype_open_flex_array(out, mangled_name, i_vert->first.second,
					optional<string>());
			}
			
			// compute and print destination name
			auto k = codeful_name(i_vert->second.as_a<array_type_die>()->get_type());
			/* FIXME: do multidimensional arrays get handled okay like this? 
			 * I reckon so, but am not yet sure. */
			string mangled_name = mangle_typename(k);
			write_uniqtype_related_array_element_type(out,
				mangled_name
			);
		}
		else if (i_vert->second.is_a<string_type_die>())
		{
			auto opt_fixed_size = i_vert->second.as_a<string_type_die>()->fixed_length_in_bytes();
			write_uniqtype_open_array(out,
				mangled_name,
				i_vert->first.second,
				(opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0)) /* pos_maxoff */,
				(opt_fixed_size ? *opt_fixed_size : 0)
			);
			/* FIXME */
			write_uniqtype_related_array_element_type(out, string("__uniqtype__unsigned_char$$8"));
		}
		else if (i_vert->second.is_a<address_holding_type_die>())
		{
			auto t = i_vert->second.as_a<address_holding_type_die>();
			pair<unsigned, iterator_df<type_die> > ultimate_pointee_pair
			 = t.as_a<address_holding_type_die>()->find_ultimate_reached_type();
			unsigned indir_level = (i_vert->first.second == string("__EXISTS1___PTR__1")) /* HACK */ ? 0
			 : ultimate_pointee_pair.first;
			auto ultimate_pointee = ultimate_pointee_pair.second;
			bool is_generic = i_vert->first.second == string("__EXISTS1___PTR__1") /* HACK */
				|| t.enclosing_cu()->is_generic_pointee_type(ultimate_pointee);
			unsigned machine_word_size = t.enclosing_cu()->get_address_size();
			bool pointee_is_codeless = false;
			if (i_vert->first.first == "") // empty summary code means we point to incomplete
			{
				pointee_is_codeless = true;
				if (ultimate_pointee.is_a<with_data_members_die>())
				{
					assert(ultimate_pointee.as_a<with_data_members_die>()->get_declaration());
					assert(*ultimate_pointee.as_a<with_data_members_die>()->get_declaration());
				}
				else
				{
					assert(ultimate_pointee.is_a<unspecified_type_die>());
					// what to do now? it's like a pointer to a fresh opaque?
				}
				//{
				//	/* HMM. Why should we get codeless subprogram types? */
				//	assert(concrete_ultimate_t.is_a<type_describing_subprogram_die>());
				//}
			}
			write_uniqtype_open_address(out,
				mangled_name,
				i_vert->first.second,
				*t->calculate_byte_size(),
				is_generic ? 0 : indir_level,
				is_generic,
				ceil(log2(machine_word_size)), /* HMM -- may be wrong on some machines */
				optional<string>(),
				/* use_section_group */ !pointee_is_codeless,
				/* emit_weak_definition */ pointee_is_codeless
			);
			// compute and print destination name
			auto k1 = codeful_name(t->get_type());
			string mangled_name1 = mangle_typename(k1);
			write_uniqtype_related_pointee_type(out, mangled_name1);
			auto k2 = codeful_name(ultimate_pointee);
			string mangled_name2 = mangle_typename(k2);
			write_uniqtype_related_ultimate_pointee_type(out, mangled_name2);
		}
		else if (i_vert->second.is_a<type_describing_subprogram_die>())
		{
			write_uniqtype_open_subprogram(out,
				mangled_name,
				i_vert->first.second,
				(opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0)) /* pos_maxoff */,
				/* narg */ fp_types.size(),
				/* nret */ 1,
				/* is_va */ 0 /* FIXME */,
				/* cc */ 0 /* FIXME */
			);
			/* Output the return type and argument types. We always output
			 * a return type, even if it's &__uniqtype__void. */
			auto return_type = i_vert->second.as_a<type_describing_subprogram_die>()->find_type();
			write_uniqtype_related_subprogram_return_type(out,
				true, mangle_typename(codeful_name(return_type)));
			
			for (auto i_t = fp_types.begin(); i_t != fp_types.end(); ++i_t)
			{
				write_uniqtype_related_subprogram_argument_type(out,
					mangle_typename(codeful_name(*i_t))
				);
				
				++contained_length;
			}
		}
		else if (i_vert->second.is_a<subrange_type_die>())
		{
			/* If we're inside an array type, we do nothing. FIXME: perhaps test
			 * for the C language too, though it's not clear whether this is a
			 * C+DWARF-specific idiom. */
			if (!i_vert->second.parent().is_a<array_type_die>())
			{
				auto base_t = i_vert->second.as_a<subrange_type_die>()->find_type();
				write_uniqtype_open_subrange(out,
					mangled_name,
					i_vert->first.second,
					(opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0)) /* pos_maxoff */,
					0 /* FIXME */,
					0 /* FIXME */
				);
				write_uniqtype_related_dummy(out);
			}
		}
		else if (i_vert->second.is_a<base_type_die>())
		{
			auto bt = i_vert->second.as_a<base_type_die>();
			
			/* As of DWARF4, we're allowed to have *either* bit_size *or* byte_size,
			 * or alternatively, both! The "both" case is already handled, and so
			 * is the "bit size only", by calculate_byte_size.
			 * we need to handle the case where the byte size
			 * must be calculated from the bit size, as ceil (bit_size / 8). */
			
			unsigned byte_size = opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0);
			pair<Dwarf_Unsigned, Dwarf_Unsigned> bit_size_and_offset = bt->bit_size_and_offset();
			unsigned bit_size = bit_size_and_offset.first;
			unsigned bit_offset = bit_size_and_offset.second;
			signed bit_size_delta = 8 * byte_size - bit_size;
			
			unsigned one_plus_log_to_use;
			signed diff_to_use;
			signed offset_to_use;
			
			if (bit_size_delta)
			{
				unsigned highest_po2_ordinal = 8 * sizeof (unsigned long) - nlz(bit_size_delta) - 1;
				unsigned next_lower_po2 = (1u<<highest_po2_ordinal);
				unsigned next_higher_po2 = (1u<<(1+highest_po2_ordinal));
				signed diff_lower = bit_size_delta - next_lower_po2; // will be positive
				signed diff_higher = bit_size_delta - next_higher_po2; // will be negative
				
				
				if (diff_lower < 128)
				{
					/* Use this one */
					one_plus_log_to_use = 1 + highest_po2_ordinal;
					diff_to_use = diff_lower;
				}
				else if (diff_higher >= -128)
				{
					one_plus_log_to_use = 2 + highest_po2_ordinal;
					diff_to_use = diff_higher;
				}
				else /* we can't represent this */
				{
					cerr << "Warning: cannot represent bit size with delta " 
						<< bit_size_delta
						<< endl;
					one_plus_log_to_use = 0;
					diff_to_use = 0;
				}
			}
			else // it's just the 8 * byte size
			{
				one_plus_log_to_use = 0;
				diff_to_use = 0;
			}
			
			// same job for the bit offset
			signed bit_offset_to_use;
			// prefer positive bit offsets, but...
			// NOTE that this will only arise if/when we have absurdly wide integers
			if (bit_offset >= 512)
			{
				if (8 * byte_size - bit_offset < 512)
				{
					// i.e. negative means "from other end"
					bit_offset_to_use = -(8 * byte_size - bit_offset); 
				}
				else
				{
					// can't represent this
					cerr << "Warning: cannot represent bit offset  " 
						<< bit_offset
						<< endl;
					bit_offset_to_use = 0;
				}
			} else bit_offset_to_use = bit_offset;
				
			write_uniqtype_open_base(out,
				mangled_name,
				i_vert->first.second,
				byte_size /* pos_maxoff */,
				i_vert->second.as_a<base_type_die>()->get_encoding(),
				one_plus_log_to_use /* one_plus_log_bit_size_delta, up to 15 i.e. delta of up to 2^15 from implied bit size */,
				diff_to_use /* bit_size_delta_delta, up to +- 127 */,
				bit_offset_to_use /* bit_offset, up to +- 512 */
			);
		
			if (needs_complement(bt))
			{
				// compute and print complement name
				auto k = make_pair(
					summary_code_to_string(
						signedness_complement_type_summary_code(
							i_vert->second
						)
					),
					name_for_complement_base_type(i_vert->second)
				);
				string mangled_name = mangle_typename(k);
				write_uniqtype_related_signedness_complement_type(out, mangled_name);
			}
			else write_uniqtype_related_dummy(out);
		}
		else if (i_vert->second.is_a<enumeration_type_die>())
		{
			write_uniqtype_open_enumeration(out,
				mangled_name,
				i_vert->first.second,
				(opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0)) /* pos_maxoff */
			);
			write_uniqtype_related_dummy(out); /* FIXME */
		}
		else if (!i_vert->second)
		{
			write_uniqtype_open_void(out,
				mangled_name,
				i_vert->first.second
			);
			write_uniqtype_related_dummy(out);
		}
		else if (i_vert->second.is_a<unspecified_type_die>())
		{
			write_uniqtype_open_void(out,
				mangled_name,
				i_vert->first.second
			);
			write_uniqtype_related_dummy(out);
		}
		else if (i_vert->second.is_a<with_data_members_die>())
		{
			write_uniqtype_open_composite(out,
				mangled_name,
				i_vert->first.second,
				(opt_sz ? (int) *opt_sz : (real_members.size() > 0 ? -1 : 0)) /* pos_maxoff */,
				members_count,
				false
			);
			unsigned i_membernum = 0;
			std::set<lib::Dwarf_Unsigned> used_offsets;
			opt<iterator_base> first_with_byte_offset;
			auto i_off = real_member_offsets.begin();
			
			// we *always* output at least one array element
			contained_length = 0;
			for (auto i_i_edge = real_members.begin(); i_i_edge != real_members.end(); ++i_i_edge, ++i_membernum, ++i_off)
			{
				++contained_length;
				auto i_edge = i_i_edge->as_a<member_die>();
				/* Instead of re-computing the canonical key,
				 * wich won't always give us the name we emitted it as
				 * (thanks to our anonymous-struct / typedef flipping HACK),
				 * search the relation for the matching concrete. But
				 * use the canonical-key lookup*/
				auto related_t = i_edge->find_or_create_type_handling_bitfields();
				string referenced_symbol_name;
				string tentative_symbol_name = mangle_typename(codeful_name(related_t));
				if (names_emitted.find(tentative_symbol_name) == names_emitted.end())
				{
					/* OK, do the slower search. */
					auto found_really = std::find_if(
						r.begin(),
						r.end(),
						[i_edge](const master_relation_t::value_type& e) -> bool {
							return e.second == i_edge->get_type()->get_concrete_type();
						}
					);
					if (found_really == r.end() ||
						names_emitted.find(mangle_typename(found_really->first)) == names_emitted.end())
					{
						cerr << "BUG: type canonically named " << tentative_symbol_name
							<< ", " << i_edge->get_type()
							<< ", concretely " << i_edge->get_type()->get_concrete_type()
							<< " was not emitted previously (found in relation?"
							<< ((found_really == r.end()) ? "no" : "yes") << ")." << endl;
						if (found_really != r.end())
						{
							for (auto i_name = names_emitted.begin(); i_name != names_emitted.end(); ++i_name)
							{
								string mangled_found = mangle_typename(found_really->first);
								// can only test for a symbol suffix match if the lengths allow...
								if (i_name->length() > mangled_found.length())
								{
									if (i_name->substr(i_name->length() - mangled_found.length()) == mangled_found)
									{
										cerr << "Possible near-miss: " << *i_name << endl;
									}
								}
							}
						}
						/* WHAT should we do here?
						 * pre-scan and output a forward decl of the uniqtype?
						 * If the caller asks for it.
						 * Then we can add its name to the already-emitted list.
						 * Problem: cycles!
						 * We've been here before.
						 * One solution was to calculate the DAG.
						 * My "easier" solution was to forward-declare everything.
						 * So presumably I need a pre-pass?
						 * Maybe we're better off doing it outside this particular
						 * function?
						 */
						//assert(false);
						referenced_symbol_name = tentative_symbol_name;
					} else referenced_symbol_name = mangle_typename(found_really->first);
				} else referenced_symbol_name = tentative_symbol_name;

				write_uniqtype_related_contained_member_type(out,
					i_i_edge == real_members.begin(),
					*i_off,
					referenced_symbol_name);
			}

			write_uniqtype_related_member_names(out,
				real_members.begin() == real_members.end(),
				emit_subobject_names ? mangled_name + "_subobj_names" : optional<string>());
		}
		else
		{
			cerr << "Saw an unknown type of tag: " <<
				i_vert->second.spec_here().tag_lookup(
					i_vert->second.tag_here()
				)
				<< endl;
			// assert(false);
		}
		
		write_uniqtype_close(out, mangled_name, contained_length);
		
		/* Output a synthetic complement if we need one. */
		if (synthesise_complements.find(i_vert->second) != synthesise_complements.end())
		{
			out << "\n/* synthesised signedness-complement type for \"" << i_vert->first.second 
				<< "\" */\n";
			// compute and print complement name
			string complement_summary_code_string = summary_code_to_string(
				signedness_complement_type_summary_code(
					i_vert->second
				)
			);
			// compl_name_pair is the *language-independent* name, e.g. "uint$32"
			auto compl_name_pair = make_pair(
				complement_summary_code_string,
				name_for_complement_base_type(i_vert->second)
			);
			string compl_name = mangle_typename(compl_name_pair);
			
			write_uniqtype_section_decl(out, compl_name);
			write_uniqtype_open_base(out, 
				compl_name,
				compl_name_pair.second,
				(opt_sz ? *opt_sz : 0),
				(i_vert->second.as_a<base_type_die>()->get_encoding() == DW_ATE_unsigned) ? 
					DW_ATE_signed : 
					(i_vert->second.as_a<base_type_die>()->get_encoding() == DW_ATE_signed) ? 
					DW_ATE_unsigned :
					i_vert->second.as_a<base_type_die>()->get_encoding(),
				0, /* FIXME */
				0, /* FIXME */
				0) /* FIXME */;
			write_uniqtype_related_signedness_complement_type(out,
				mangled_name
			);
			write_uniqtype_close(out, compl_name, 1);
			
			/* If our actual type has a C-style name, output a C-style alias for the 
			 * complement we just output. For example, if we are processing a binary
			 * that uses "int" but not "unsigned int", we'll just have emitted
			 * "uint$32" so should emit its alias "unsigned int".
			 *
			 * FIXME: how *should* this work? Who consumes  these aliases?
			 * Is it only our sloppy-dumptypes test case, i.e. typename-
			 * -based client code that expects C-style names? 
			 * 
			 * In general we want to factor this into a pair of extra phases in allocscc:
			 * one which "lowers" trumptr-generated typenames into canonical
			 * language-independent ones, 
			 * and one which "re-aliases them" in language-dependent form. We could use
			 * this to support e.g. Fortran at the same time as C, etc..
			 * BUT NOTE that the "language-dependent" form is, in general, both language-
			 * and *compiler*-dependent, i.e. more than one base type might be "unsigned long"
			 * depending on compiler flags etc..
			 * */
			if (i_vert->second.name_here())
			{
				const char **equiv = abstract_c_compiler::get_equivalence_class_ptr(
					type_die_get_name(i_vert->second)->c_str());
				if (equiv)
				{
					// we're outputting a type with a C-style name, i.e. matching one of
					// the known equivalence classes. FIXME: only check this for DIES in
					// C-language compilation units!
					bool is_unsigned = (string(equiv[0]).find("unsigned") != string::npos);
					// Find the matching equivalence class in the top-level array thereof.
					// This means we are iterating through an array of pointer to equiv class
					const char ** const* found_equiv = std::find(
						abstract_c_compiler::base_typename_equivs,
						abstract_c_compiler::base_typename_equivs_end,
						equiv
					);
					assert(found_equiv);
					// Find the complementary equivalence class.
					// equiv classes are {s, u, s, u, ...}
					const char **compl_equiv = is_unsigned ? found_equiv[-1]  : found_equiv[+1];
					// We take the *first* member of that equivalence class. FIXME: should be same-pos member?
					auto complement_c_style_name_pair = make_pair(complement_summary_code_string, compl_equiv[0]);
					// alias our complement under that name.
					emit_weak_alias_idem(out, mangle_typename(complement_c_style_name_pair),
						/* existing name */ mangle_typename(compl_name_pair));
					// we just emitted a definition for "unsigned int" or whatever,
					// so remember it in case there is some ambiguity over the codeless alias
					name_pairs_by_name[compl_equiv[0]].insert(compl_name_pair /* i.e. language-independent */);
					/* If this is a DIE that we don't consider for aliasing, add it to the
					 * blacklist. This mostly means bitfield types. FIXME: should also roll
					 * 'depends on incomplete' into this? */
					if (avoid_aliasing_as(compl_equiv[0], i_vert->second /* the DIE */))
					{
						// the blacklist just remembers the summary code
						codeless_alias_blacklist[compl_equiv[0]].insert(complement_c_style_name_pair.first);
					}
				}
			}
		}
		
		/* Output any (typedef-or-base-type) aliases for this type. NOTE that here we are
		 * assuming that the canonical name for any base type (used above) is not the same as its
		 * programmatic name (aliased here), e.g. "uint$32" does not equal "unsigned int".
		 *
		 * FIXME: this is emitting alias cycles currently. I think this is down to
		 * changes that added find_associated_name in libdwarfpp. What has changed?
		 * Do we use a libdwarfpp call to get canonical names? YES,
		 * 			string canonical_typename = dwarf::core::abstract_name_for_type(t);
		 * in get_types_by_codeless_uniqtype_name.
		 *
		 * Did this change to use the new 'associated names'?
		 * Not that I can see.
		 *
		 *  in add_type_if_absent(iterator_df<type_die> t, master_relation_t& r)
		 *     we add aliases when concrete_t != t, covering typedefs
		 *  ... calling type_die_get_name to get the alias name
		 *  and calling add_alias_if_absent to do the deed.
		 *
		 * THEN in
		 * void make_exhaustive_master_relation(master_relation_t& rel, 
		    dwarf::core::iterator_df<> begin, 
		    dwarf::core::iterator_df<> end)
		 *  we do the swapping thing:

			// Are with a with-data-members DIE (i.e. "normally" we'd have a name),
			// with no source-level name,
			// with a unique alias that does have a source-level name?
			if (i_rel->second && i_rel->second.is_a<with_data_members_die>()
				&& !i_rel->second.name_here()
				&& rel.aliases[i_rel->first].size() == 1)
			{
				master_relation_t::value_type v = *i_rel;
				codeful_name initial_key = v.first;
				string unique_alias = *rel.aliases[initial_key].begin();
				codeful_name actual_key = make_pair(initial_key.first, // code
					unique_alias); // uniqtype name
				std::cerr << "Swapping '" << initial_key.second
					<<  "' and '" << unique_alias << "'" << std::endl;
				i_rel = rel.erase(i_rel);

		 * So what has changed that's causing our alias thing?
		 * Is it that the typedef is still getting an alias, but also
		 * the anonymous struct that it refers to now has exactly the same name?
		 * It presumably isn't *just* that because the tests used to pass
		 * with the swapping thing in action.
		 *
		 * type_die_get_name just does name_here()
		 * Where does find_associated_name() get used? It gets used from arbitrary_name
		 * but we don't seem to use that.
		 * It also gets used in may_equal, abstractly_equals and the summary code
		 * and in with_data_members_die::print_abstract_name
		 * and dwarf::core::abstract_name_for_type
		 * AHA
		 * and initial_key_for_type(iterator_df<type_die> t) uses abstract_name
		 * AND abstract_name is now the associated ("swapped") name for such anonymous structs.
		 *
		 * So the "!i_rel->second.name_here()" is not the right test
		 * because in reality the name that will be used (via abstract_name())
		 * is not an anonymous autogenerated name
		 * but the actual typedef'd name, setting up the exact same symbol name.
		 
		 * We might want to skip the swapping, but that's not enough because
		 * swapping the same alias with itself will still leave a duplicate!
		 * Instead we need to avoid creating the alias, by using the initial key function
		 * to generate the name that we test for duplicates.
		 */
		for (auto i_alias = r.aliases[i_vert->first].begin(); 
			i_alias != r.aliases[i_vert->first].end();
			++i_alias)
		{
			emit_weak_alias_idem(out,
				mangle_typename(make_pair(i_vert->first.first, *i_alias)),
				mangle_typename(i_vert->first),
				/* emit section? */ i_vert->first.first != ""
			);
			types_by_name[*i_alias].insert(i_vert->second);
			name_pairs_by_name[*i_alias].insert(i_vert->first);
			/* This is mostly to avoid codeless aliases for bitfield types. */
			if (avoid_aliasing_as(*i_alias, i_vert->second))
			{
				codeless_alias_blacklist[*i_alias].insert(i_vert->first.first);
			}
		}
	}
	
	/* Codeless aliases: linker aliases for any concrete typenames *or* typedef names 
	 * that were uniquely defined. */
	if (emit_codeless_aliases)
	{
		out << "/* Begin codeless (__uniqtype__<typename>) aliases. */" << endl;
		for (auto i_set_by_name = name_pairs_by_name.begin(); i_set_by_name != name_pairs_by_name.end();
			++i_set_by_name)
		{
			string name = i_set_by_name->first;
			std::vector<const pair<string, string> *> aliases_to_consider;
			for (auto i_pair = i_set_by_name->second.begin(); i_pair != i_set_by_name->second.end();
				++i_pair)
			{
				const string& codestr = i_pair->first;
				if (codeless_alias_blacklist[name].find(codestr)
						== codeless_alias_blacklist[name].end())
				{
					aliases_to_consider.push_back(&*i_pair);
				}
			}
			
			if (aliases_to_consider.size() == 1)
			{
				/* This name only denotes one type, so we can alias it if it's complete. */
				auto& full_name_pair = **aliases_to_consider.begin();
				if (full_name_pair.first != "")
				{
					string full_name = mangle_typename(full_name_pair);
					pair<string, string> abbrev_name_pair = make_pair("", i_set_by_name->first);
					string abbrev_name = mangle_typename(abbrev_name_pair);
					emit_weak_alias_idem(out,
						mangle_typename(abbrev_name_pair),
						cxxgen::escape(full_name) /* FIXME: mangling looks suspect here */
					);
				}
			}
			else
			{
				out << "/* No unique meaning for alias \"" << i_set_by_name->first << "\"; set is {";
				for (auto i_t = aliases_to_consider.begin(); i_t != aliases_to_consider.end(); ++i_t)
				{
					if (i_t != aliases_to_consider.begin()) cout << ",";
					out << "\n\t" << mangle_typename(**i_t);
				}

				out << "\n} (blacklisted: {";
				bool emitted = false;
				for (auto i_pair = i_set_by_name->second.begin(); i_pair != i_set_by_name->second.end();
					++i_pair)
				{
					for (auto i_str = codeless_alias_blacklist[name].begin();
						i_str != codeless_alias_blacklist[name].end(); ++i_str)
					{
						if (emitted) cout << ", ";
						out << *i_str;
						emitted = true;
					}
				}
				out << " }) */" << endl;
			}
		}
	}
}

void write_uniqtype_section_decl(std::ostream& o, const string& mangled_typename)
{
	/* SRK notes: I've disabled this since it doesn't actually seem to
	 * work. If I use this, I get sections with duplicate names but
	 * different flags, and only the empty one (created by these decls,
	 * but immediately jumped away from) gets the correct awG flags.
	 */
#if 0
	o << "__asm__(\".section .data." << mangled_typename
	  << ", \\\"awG\\\", @progbits, " << mangled_typename << ", comdat"
	  << "\\n\\t.previous\");\n";
#endif
}

static void write_uniqtype_open_generic(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	const string& pos_maxoff_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	o << "struct uniqtype " << mangled_typename
	  << attributes_for_uniqtype(mangled_typename, make_weak_definition, use_section_group)
	  << " = {\n\t"
	  << "{ 0, 0, 0 },\n\t"
	  << pos_maxoff_str << " /* pos_maxoff */,\n\t";
}

static void write_uniqtype_open_generic(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	bool use_section_group = true,
	bool make_weak_definition = false
	)
{
	std::ostringstream s; s << pos_maxoff;
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, s.str(),
		use_section_group, make_weak_definition);
}

void write_uniqtype_open_void(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	optional<string> maxoff_comment_str
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, 0);
	o << "{ _void: { VOID } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_array(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	unsigned nelems,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff);
	o << "{ array: { 1, " << nelems << " } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_flex_array(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	std::ostringstream s; s << UNIQTYPE_POS_MAXOFF_UNBOUNDED;
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, s.str());
	o << "{ array: { 1, " << UNIQTYPE_ARRAY_LENGTH_UNBOUNDED << " } },\n\t"
		<< "/* make_precise */ __liballocs_make_array_precise_with_memory_bounds, /* related */ {\n\t\t";
}
void write_uniqtype_open_address(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	unsigned indir_level,
	bool is_generic,
	unsigned log_min_align,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff,
		use_section_group, make_weak_definition);
	o << "{ address: { ADDRESS, " << indir_level << ", " << is_generic << ", " << log_min_align
		<< " } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_base(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	unsigned enc,
	unsigned one_plus_log_bit_size_delta,
	signed bit_size_delta_delta,
	signed bit_off,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff, use_section_group, make_weak_definition);
	o << "{ base: { BASE, " << enc
		<< ", " << one_plus_log_bit_size_delta
		<< ", " << bit_size_delta_delta
		<< ", " << bit_off
		<< " } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_subrange(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	signed min,
	signed max,
	optional<string> comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff, use_section_group, make_weak_definition);
	o << "{ subrange: { SUBRANGE, " << min
		<< ", " << max
		<< " } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_enumeration(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff, use_section_group, make_weak_definition);
	o << "{ enumeration: { ENUMERATION, 0, 0, 0 } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_composite(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	unsigned nmemb,
	bool not_simultaneous,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff, use_section_group, make_weak_definition);
	o << "{ composite: { COMPOSITE, " << nmemb
		<< ", " << not_simultaneous 
		<< " } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
}
void write_uniqtype_open_subprogram(std::ostream& o,
	const string& mangled_typename,
	const string& unmangled_typename,
	unsigned pos_maxoff,
	unsigned narg,
	unsigned nret,
	bool is_va,
	unsigned cc,
	optional<string> maxoff_comment_str,
	bool use_section_group,
	bool make_weak_definition
	)
{
	write_uniqtype_open_generic(o, mangled_typename, unmangled_typename, pos_maxoff, use_section_group, make_weak_definition);
	o << "{ subprogram: { SUBPROGRAM, " << narg 
		<< ", " << nret 
		<< ", " << is_va
		<< ", " << cc
		<< " } },\n\t"
		<< "/* make_precise */ (void*)0, /* related */ {\n\t\t";
	
}
void write_uniqtype_related_array_element_type(std::ostream& o,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	/* begin the struct */
	o << "{ { t: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_pointee_type(std::ostream& o,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	/* begin the struct */
	o << "{ { t: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_ultimate_pointee_type(std::ostream& o,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	o << ",\n\t\t";
	/* begin the struct */
	o << "{ { t: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_subprogram_argument_type(std::ostream& o,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	o << ",\n\t\t";
	/* begin the struct */
	o << "{ { t: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_subprogram_return_type(std::ostream& o,
	bool is_first,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	if (!is_first) o << ",\n\t\t";
	/* begin the struct */
	o << "{ { t: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_contained_member_type(std::ostream& o,
	bool is_first,
	unsigned offset,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	if (!is_first) o << ",\n\t\t";
	/* begin the struct */
	o << "{ { memb: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << ", " << offset << ", 0, 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_member_names(std::ostream& o,
	bool is_first,
	optional<string> maybe_subobj_names,
	optional<string> comment_str
	)
{
	if (!is_first) o << ",\n\t\t";
	/* begin the struct */
	o << "{ { memb_names: { ";
	if (maybe_subobj_names) o << *maybe_subobj_names;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_signedness_complement_type(std::ostream& o,
	optional<string> maybe_mangled_typename,
	optional<string> comment_str
	)
{
	/* begin the struct */
	o << "{ { t: { ";
	if (maybe_mangled_typename) o << "&" << *maybe_mangled_typename;
	else o << "(void*) 0";
	o << " } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}
void write_uniqtype_related_dummy(std::ostream& o,
	optional<string> comment_str
	)
{
	/* begin the struct */
	o << "{ { t: { (void*) 0 } } }";
	if (comment_str) o << " /* " << *comment_str << " */ ";
}

void write_uniqtype_close(std::ostream& o, const string& mangled_name, optional<unsigned> n_contained)
{
	o << "\n\t}";
	o << "\n};\n";
	if (n_contained) o << ensure_contained_length(mangled_name, *n_contained);
}

static void for_each_uniqtype_reference_in(const string &filename,
	std::function<void(const string&)> f)
{
	FILE *in = popen((string("nm -fposix -u '") + filename
	 + "' | sed -r 's/[[:blank:]]*[Uw][[:blank:]]*$//' | grep __uniqtype").c_str(), "r");
	assert(in);
	
	int ret;
	char *line = NULL;
	size_t line_len;
	/* Now popen our input, read lines and match them against the map we just built. */
	while (ret = getline(&line, &line_len, in), ret > 0)
	{
		string key(line);
		// trim the newline, if any
		boost::trim(key);
		f(key);
		
		free(line);
		line = NULL;
	}
	fclose(in);
}
int dump_usedtypes(const vector<string>& fnames, std::ostream& out, std::ostream& err,
	bool continue_on_error /* = false */)
{
	using core::root_die;
	using std::unique_ptr;
	
	std::vector<std::unique_ptr<std::ifstream> > infstreams(fnames.size()); 
	std::vector<std::unique_ptr<root_die> > rs(fnames.size());
	
	/* The codeless map, alias map and and master relation are shared across 
	 * *all* files that we process. */
	multimap<string, iterator_df<type_die> > types_by_codeless_uniqtype_name;
	master_relation_t master_relation;
	multimap<string, pair<string, string> > aliases_needed;
	
	for (unsigned i = 0; i < fnames.size(); ++i)
	{
		const string& fname = fnames.at(i);
		infstreams[i] = std::move(unique_ptr<std::ifstream>(new std::ifstream(fname)));
		std::ifstream& infstream = *infstreams[i];
		if (!infstream) 
		{
			err << "Could not open file " << fname << endl;
			if (continue_on_error) continue; else return 1;
		}

		try
		{
			rs[i] = std::move(unique_ptr<root_die>(new root_die(fileno(infstream))));
		}
		catch (lib::No_entry)
		{
			rs[i] = nullptr;
			continue; // it's never an error to contain no DWARF
		}
		root_die &r = *rs[i];

		try
		{
			get_types_by_codeless_uniqtype_name(types_by_codeless_uniqtype_name, 
				r.begin(), r.end());
		}
		catch (lib::Error)
		{
			err << (continue_on_error ? "Warning:" : "Error:")
			    << " could not process DWARF types for file " << fnames[i] << endl;
			rs[i] = nullptr;
			if (continue_on_error) continue; else return 2;
		}
		
		auto f = [&](const string& key) {
			// FIXME: escape single quotes
			auto found_pair = types_by_codeless_uniqtype_name.equal_range(key);
			unsigned found_count = srk31::count(found_pair.first, found_pair.second);
		
			switch (found_count)
			{
				case 0:
					err << "Found no match for " << key << endl;
					/* HACK around CIL brokenness: if we contain the string 
					 *     "__FUN_FROM___FUN_TO_" 
					 * then match against
					 *     "__FUN_FROM___VA___FUN_TO_" 
					 * since CIL typesigs don't distinguish between 
					 * "no specified parameters"        e.g. int f() 
					 * and "specified as no parameters" e.g. int f(void).
					 * 
					 * This will ensure that some type gets emitted, such that we
					 * can bind up the UNDefined uniqtype to it. 
					 * BUT
					 * We will emit it under its rightful name, so the reference
					 * won't get bound just like that. Previously we dealt with
					 * this by creating an alias, but in fact we need to emit
					 * the uniqtype *again* under the correct section name. 
					 * Otherwise the name we want might get eliminated by COMDAT
					 * if a non-worked-around section appears in the same link.
					 */
					{
						string search_expr = "__FUN_FROM___FUN_TO_";
						string replace_expr = "__FUN_FROM___VA___FUN_TO_";
						string::size_type pos = key.find(search_expr);
						if (pos != string::npos)
						{
							string substitute_key = key;
							substitute_key.replace(pos, search_expr.size(), replace_expr);

							auto found_retry_pair = types_by_codeless_uniqtype_name.equal_range(substitute_key);
							if (found_retry_pair.first != found_retry_pair.second)
							{
								err << "Working around CIL bug by substituting " << substitute_key << endl;
								auto name_pair = transitively_add_type(found_retry_pair.first->second, master_relation).second;

								string orig_substitute_key = substitute_key;

								substitute_key.replace(0, string("__uniqtype_").size(), "__uniqtype_" + name_pair.first);

								string orig_key_symname = key;
								orig_key_symname.replace(0, string("__uniqtype_").size(), "__uniqtype_" + name_pair.first);

								aliases_needed.insert(make_pair(orig_key_symname, make_pair(orig_substitute_key, substitute_key)));
								break;
							}
						}
					}
					err << "Defined are: ";
					for (auto i_tname = types_by_codeless_uniqtype_name.begin(); i_tname != types_by_codeless_uniqtype_name.end(); ++i_tname)
					{
						if (i_tname != types_by_codeless_uniqtype_name.begin()) err << ", ";
						err << i_tname->first;
					}
					err << endl;
					return;
				case 1: 
					// out << "Found match for " << key << ": " << found_pair.first->second << endl;
					transitively_add_type(found_pair.first->second, master_relation);
					break;

				default: 
					cerr << "Found multiple matches (" << found_count << ") for " << key << ": " << endl;
					auto first_found = found_pair.first;
					multimap<opt<uint32_t>, decltype(found_pair.first)> by_code;
					for (auto i_print = found_pair.first; i_print != found_pair.second; ++i_print)
					{
						auto code = type_summary_code(i_print->second);
						by_code.insert(make_pair(code, i_print));
						cerr << "\t" 
							<< i_print->second
							<< " (code: " 
							<< summary_code_to_string(code) 
							<< ")" << endl;
					}
					/* Do they all seem to be identical? */
					auto range_equal_to_first = by_code.equal_range(type_summary_code(first_found->second));
					if (srk31::count(range_equal_to_first.first, range_equal_to_first.second)
					 == found_count)
					{
						auto code = type_summary_code(first_found->second);
						cerr << "They all seem to be identical (code " 
							<< (code ? *code : -1)
							<< ") so proceeding." << endl;
						transitively_add_type(first_found->second, master_relation);
					}
					else 
					{
						cerr << "Not identical, so not proceeding." << endl;
						return;
					}
				// end case default
			}
		};
		
		for_each_uniqtype_reference_in(fname, f);
		
	} // end for each input file

	// write the types to stdout
	set<string> names_emitted;
	map<string, set< iterator_df<type_die> > > types_by_name;
	map< iterator_df<type_die>, set<string> > names_by_type;
	write_master_relation(master_relation, out, cerr,
		names_emitted, types_by_name, /* subobject names */ true);
	
	// for CIL workaround: for each alias, write a one-element master relation
	// defining it under the alias name (do *not* use the other name at all!)
	for (auto i_pair = aliases_needed.begin(); i_pair != aliases_needed.end(); ++i_pair)
	{
		//out << "extern struct uniqtype " << i_pair->first << " __attribute__((alias(\"" << i_pair->second << "\")));"
		// 	<< endl;
		
		// i_pair is (orig_key_symname, (orig_substitute_key, substitute_key_with_typecode))
		// and we need to look up in types_by_uniqtype_name by orig_substitute_key
		
		auto found = types_by_codeless_uniqtype_name.equal_range(i_pair->second.first);
		assert(found.first != found.second || (cerr << i_pair->second.first << endl, false));
		
		master_relation_t tmp_master_relation;
		string unmangled_name = i_pair->first;
		unmangled_name.replace(0, string("__uniqtype_........_").size(), "");
		string insert = i_pair->first.substr(string("__uniqtype_").size(), 8);
		tmp_master_relation.insert(make_pair(codeful_name(insert, unmangled_name), found.first->second));
		
		set<string> tmp_names_emitted;
		map<string, set< iterator_df<type_die> > > tmp_types_by_name;
		write_master_relation(tmp_master_relation, out, err,
			tmp_names_emitted, tmp_types_by_name, true /* subobject names */);
	}
	
	return 0;
}

string summary_code_to_string(opt<uint32_t> maybe_code)
{
	if (!maybe_code) return "";
	uint32_t code = *maybe_code;
	ostringstream summary_string_str;
	summary_string_str << std::hex << std::setfill('0') << std::setw(2 * sizeof code) << code 
		<< std::dec;
	return summary_string_str.str();
}
string
name_for_complement_base_type(iterator_df<base_type_die> base_t)
{
	/* For base types, we use our own language-independent naming scheme. */
	ostringstream name;
	unsigned size = *base_t->get_byte_size();
	auto encoding = base_t->get_encoding();
	assert(encoding == DW_ATE_signed || encoding == DW_ATE_unsigned);
	pair<Dwarf_Unsigned, Dwarf_Unsigned> bit_size_and_offset = base_t->bit_size_and_offset();
	bool needs_suffix = !((bit_size_and_offset.second == 0) 
		&& (bit_size_and_offset.first == 8 * size));
	/* Single-'$' is appropriate because we will mangle this separately. */
	name << ((base_t->get_encoding() == DW_ATE_signed) ? "uint" : "int")
		<< "$" << bit_size_and_offset.first;
	if (needs_suffix) name << "$" << bit_size_and_offset.second;

	return name.str();
}
#define CANONICAL_NAME_FOR_TYPE(t, self_call, empty_string_type, string_concat, do_return, \
   get_canonical, \
   is_void, is_base, get_base_encoding, get_base_byte_size, get_base_canonical_name, \
   is_fun, is_fun_va, for_each_fun_arg_type, for_each_fun_ret_type, \
   is_ptr, \
   is_array, get_array_element_type, get_array_element_count_str, \
   is_str, get_str_element_count_str, get_str_element_size_str, \
   is_addr, get_addr_target, get_addr_flavour_name, \
   has_explicit_name, get_explicit_name, make_name_for_anonymous, \
   handle_default_case, trace) \
do { \
	if (is_void(t) || is_void(get_canonical(t))) { trace("void"); do_return("void", t); } \
	t = get_canonical(t); \
	if (is_base(t)) \
	{ \
		trace("base"); \
		if (get_base_encoding(t) == 0) { \
			assert(get_base_byte_size(t) == 1); \
			do_return("__uninterpreted_byte", t); \
		} \
		do_return(get_base_canonical_name(t), t); \
	} \
	if (is_fun(t)) \
	{ \
		trace("fun"); \
		empty_string_type s; \
		string_concat(s, "__FUN_FROM_"); \
		/* We know we want to concat the arg insert. User tells us how to get the arg insert. */ \
		/* ... and how to loop over args. So who writes the body of the loop? We do. */ \
		for_each_fun_arg_type(t, __argt, __argnstr) \
		{ string_concat(s, string_concat(string_concat(string_concat(string("__ARG"), __argnstr), "_"), self_call(__argt))); } \
		if (is_fun_va(t)) { string_concat(s, "__VA_"); } \
		string_concat(s, "__FUN_TO_"); \
		for_each_fun_ret_type(t, __rett, __retnstr) { string_concat(s, self_call(__rett)); } \
		do_return(s, t); \
	} \
	if (is_array(t)) \
	{ \
		trace("array"); \
		do_return(string_concat( \
		    string_concat(string_concat(string("__ARR"), get_array_element_count_str(t)), "_"), \
		        self_call(get_array_element_type(t)) \
		), t); \
	} \
	if (is_str(t)) \
	{ \
		trace("str"); \
		do_return(string_concat( \
		    string_concat(string_concat(string("__STR"), get_str_element_count_str(t)), "_"), \
		        get_str_element_size_str(t) \
		), t); \
	} \
	if (is_addr(t)) \
	{ \
		trace("addr"); \
		do_return(string_concat(get_addr_flavour_name(t), self_call(get_addr_target(t))), t); \
	} \
	if (has_explicit_name(t)) \
	{ \
		trace("named"); \
		do_return(get_explicit_name(t), t); \
	} \
	trace("default"); \
	do_return(make_name_for_anonymous(t), t); \
} while (0)

/* We now have three implementations of getting the canonical name for a DIE.
 * - the "old" version formerly used in the function below, now evolved into
	    the big macro above
 * - the one in libdwarfpp
 *     (which got there because canonical names are useful for defining
 *      the strongly-connected components algorithms, and to make it easy
 *      to cache SCCs when computed, it made sense to put it directly on the
 *      DIE types)
 " - the "new" heavily macroised version below. The idea of the macroised
 *     version is to support a C implementation of the logic, implemented
 *     over uniqtypes, and the present C++ one implemented over DIEs.
 *     We will want the C version if we put any of this into the liballocs
 *     runtime (for dynamically generated uniqtypes). Currently we just
 *     use it here, and assert that it gives the same answer as libdwarfpp version.
 *
 * Because liballocstool no longer uses strictly 'canonical' names, but rather
 * allows some deviations (for anonymous structs with a unique typedef alias),
 * we don't provide this function any more. Some code still calls the libdwarfpp
 * version. To keep our macro compile-tested, we still compile this code by
 * putting it in an anonymous namespace.
 */
namespace {
string canonical_name_for_type(iterator_df<type_die> t)
{
	auto get_canonical = [](iterator_df<type_die> t) { return !t ? t : t->get_concrete_type(); };
	auto is_void = [](iterator_df<type_die> t) {
		return !t || !t->get_concrete_type() ||
		// FIXME: not the right semantics probably
		t.is_a<unspecified_type_die>();
	};
	auto is_base = [](iterator_df<type_die> t) { return t.is_a<base_type_die>(); };
	auto get_base_encoding = [](iterator_df<type_die> t)
	{ return t.as_a<base_type_die>()->get_encoding(); };
	auto get_base_byte_size = [](iterator_df<type_die> t)
	{ return t.as_a<base_type_die>()->get_byte_size(); };
	auto get_base_canonical_name = [](iterator_df<type_die> t)
	{ return t.as_a<base_type_die>()->get_canonical_name(); };
	auto is_fun = [](iterator_df<type_die> t)
	{ return (t.is_a<subroutine_type_die>() || t.is_a<subprogram_die>()); };
	#define dwarfpp_for_each_fun_arg_type(t, __argt, __argnstr) \
		auto fps = t.children().subseq_of<formal_parameter_die>(); \
		unsigned argnum = 0; \
		string __argnstr = "0"; \
		std::ostringstream nstr; \
		iterator_df<type_die> __argt = (fps.first != fps.second) ? (fps.first->find_type()) : iterator_df<type_die>(iterator_base::END); \
		for (auto i_fp = fps.first; i_fp != fps.second; \
		   ++i_fp, __argt = (i_fp != fps.second) ? (i_fp->find_type()) : iterator_df<type_die>(iterator_base::END), \
		   ++argnum, __argnstr = (nstr.clear(), nstr.str(""), (nstr << argnum), nstr.str()))
	#define dwarfpp_for_each_fun_ret_type(t, __rett, __retnstr) \
	    iterator_df<type_die> __rett = (RETURN_TYPE(t)) ? (iterator_df<type_die>(RETURN_TYPE(t)))->get_concrete_type() : (iterator_base::END); \
	    string __retnstr = "0";
	auto is_array = [](iterator_df<type_die> t) { return t.is_a<array_type_die>(); };
	auto get_array_element_type = [](iterator_df<type_die> t)
	{ return t.as_a<array_type_die>()->find_type(); };
	auto self_call = [=](iterator_df<type_die> t) { return canonical_name_for_type(t); };
	auto get_array_element_count_str = [](iterator_df<type_die> t)
	{
		/* What should the type descriptor for "array of n T" look like? 
		 * What should it be called? 
		 * Answers: always has exactly one nmemb, and use __ARRn_. */
		
		/* What should the type descriptor for "array of undeterminedly-many T" look like?
		 * What should it be called? Answer: use __ARR_*/
		
		/* How do we encode mutual recursion between array and pointer?
		 * Answer: nothing special: just cut off the array first part and emit it specially,
		 * with a reference to the remainder (what it's an array of).
		 * This handles multidimensional arrays too.
		 */
		ostringstream array_prefix;
		opt<Dwarf_Unsigned> element_count = t.as_a<array_type_die>()->element_count();
		if (element_count) array_prefix << *element_count;
		return array_prefix.str();
	};
	auto is_str = [](iterator_df<type_die> t) { return t.is_a<string_type_die>(); };
	auto get_str_element_size_str = [](iterator_df<type_die> t) { return "1"; /* FIXME */};
	auto get_str_element_count_str = [](iterator_df<type_die> t)
	{
		ostringstream str_prefix;
		const Dwarf_Unsigned element_size = 1; /* FIXME: always 1? */
		opt<Dwarf_Unsigned> opt_byte_size = t.as_a<string_type_die>()->fixed_length_in_bytes();
		opt<Dwarf_Unsigned> element_count
		 = opt_byte_size ? opt<Dwarf_Unsigned>(*opt_byte_size / element_size ) : opt<Dwarf_Unsigned>();
		str_prefix << (element_count ? *element_count : 0);
		return str_prefix.str();
	};
	auto is_addr = [](iterator_df<type_die> t) { return t.is_a<address_holding_type_die>(); };
	auto get_addr_flavour_name = [](iterator_df<type_die> t) -> std::string
	{
		switch (t.tag_here())
		{
			case DW_TAG_pointer_type: return "__PTR_";
			case DW_TAG_reference_type: return "__REF_";
			case DW_TAG_rvalue_reference_type: return "__RR_";
			default:
				assert(false);
		}
	};
	auto get_addr_target = [](iterator_df<type_die> t)
	{ return t.as_a<address_holding_type_die>()->find_type(); };
	auto has_explicit_name = [](iterator_df<type_die> t) -> bool { return t.name_here(); };
	auto get_explicit_name = [](iterator_df<type_die> t) -> std::string {
		// const char *ret = (t.name_here()) ? (*type_die_get_name(t)).c_str() : NULL;
		// return ret;
		// for strange reasons, the above doesn't work. I think it's a lifetime thing.
		return string(*type_die_get_name(t));
	};
	auto make_name_for_anonymous = [](iterator_df<type_die> t) {
		string name_to_use;
		string offsetstr = offset_to_string(t.offset_here());
		/* We really want to allow deduplicating anonymous structure types
		 * that originate in the same header file but are included in multiple
		 * compilation units. Since each gets a different offset, using that
		 * for the fake name string is a bad idea. Instead, use the defining
		 * source file path, if we have it. */
		if (t->get_decl_file() && t->get_decl_line())
		{
			ostringstream s;
			opt<string> maybe_fqp = t.enclosing_cu()->source_file_fq_pathname(*t->get_decl_file());
			s << (maybe_fqp ?
				boost::filesystem::path(*maybe_fqp).filename() : 
				boost::filesystem::path(t.enclosing_cu()->source_file_name(*t->get_decl_file())).filename())
				<< "_" << *t->get_decl_line();
			name_to_use = s.str();
		}
		else name_to_use = offsetstr;
		return name_to_use;
	};
	#define empty_string_type std::string
	#define string_concat(s1, s2) ((s1) += (s2))
	#define do_return(s, t) do { string libdwarfpp_said = abstract_name_for_type(t); \
	     if (!t.is_a<subrange_type_die>() && s != libdwarfpp_said) warnx("mismatch: %s vs libdwarfpp %s", string(s).c_str(), libdwarfpp_said.c_str()); \
	     assert(t.is_a<subrange_type_die>() || s == libdwarfpp_said); return s; } while(0)
	#define handle_default_case(t) do { assert(false); abort(); } while(0)
	auto trace = [](const char *str) { /* warnx("It's: %s", str); */ };
	CANONICAL_NAME_FOR_TYPE(t, self_call, empty_string_type, string_concat, do_return,
		get_canonical,
		is_void, is_base, get_base_encoding, get_base_byte_size, get_base_canonical_name,
		is_fun, IS_VARIADIC, dwarfpp_for_each_fun_arg_type, dwarfpp_for_each_fun_ret_type, is_ptr,
		is_array, get_array_element_type, get_array_element_count_str,
		is_str, get_str_element_count_str, get_str_element_size_str,
		is_addr, get_addr_target, get_addr_flavour_name,
		has_explicit_name, get_explicit_name, make_name_for_anonymous,
		handle_default_case, trace);
}
} /* end anonymous namespace */

string codestring_for_type(iterator_df<type_die> t)
{
	if (!t) return "";
	t = t->get_concrete_type();
	if (!t) return "";

	opt<uint32_t> code = type_summary_code(t);
	string summary_string;
	if (code)
	{
		summary_string = summary_code_to_string(*code);
		assert(summary_string.size() == 2 * sizeof *code);
	} else summary_string = "";
	return summary_string;
}

// iterator_df<type_die>
// find_type_in_cu(iterator_df<compile_unit_die> cu, const string& name)
// {
// 	/* For the most part, we just do named_child.
// 	 * BUT, for base types, we widen the search, using our equivalence classes. */
// 	for (const char **const *p_equiv = &abstract_c_compiler::base_typename_equivs[0]; *p_equiv != NULL; ++p_equiv)
// 	{
// 		for (const char **p_el = p_equiv[0]; *p_el != NULL; ++p_el)
// 		{
// 			if (name == string(*p_el))
// 			{
// 				/* We try every element in the class */
// 				for (const char **i_attempt = p_equiv[0]; *i_attempt != NULL; ++i_attempt)
// 				{
// 					iterator_df<type_die> found = cu.named_child(string(*i_attempt));
// 					if (found != iterator_base::END) return found;
// 				}
// 			}
// 		}
// 	}
// 
// 	// if we got here, just try named_child
// 	return iterator_df<type_die>(cu.named_child(name)); //shared_ptr<type_die>();
// }

opt<uint32_t> type_summary_code(core::iterator_df<core::type_die> t)
{
	if (!t) return opt<uint32_t>(0);
	else return t->summary_code();
}
opt<uint32_t> signedness_complement_type_summary_code(core::iterator_df<core::base_type_die> base_t)
{
	unsigned encoding = base_t->get_encoding();
	assert(encoding == DW_ATE_signed || encoding == DW_ATE_unsigned);
	dwarf::core::summary_code_word<uint32_t> output_word;
	assert(base_t->get_byte_size());
	unsigned byte_size = *base_t->get_byte_size();
	pair<Dwarf_Unsigned, Dwarf_Unsigned> bit_size_and_offset = base_t->bit_size_and_offset();
	unsigned bit_size = bit_size_and_offset.first;
	unsigned bit_offset = bit_size_and_offset.second;
	output_word << DW_TAG_base_type 
		<< (encoding == DW_ATE_unsigned ? DW_ATE_signed : DW_ATE_unsigned) 
		<< byte_size << bit_size << bit_offset;
	return output_word.val;
}	

void get_types_by_codeless_uniqtype_name(
	std::multimap<string, iterator_df<type_die> >& m, 
	iterator_df<> begin, iterator_df<> end)
{	
	/* First we look through the whole file and index its types by their *codeless*
	 * *canonical* uniqtype name, i.e. we blank out the first element of the name pair. */
	bool done_some_output = false;
	for (iterator_df<> i = begin; i != end; ++i)
	{
		if (i.is_a<type_die>())
		{
			if (isatty(fileno(std::cerr)))
			{
				if (done_some_output) std::cerr << "\r";
				std::cerr << "Codeless uniqtypes: adding DIE at 0x" << std::hex << i.offset_here() << std::dec;
				done_some_output = true;
			}
			opt<string> opt_name = i.name_here(); // for debugging
			if (opt_name)
			{
				string name = *opt_name;
				assert(name != "");
			}
			
			auto t = i.as_a<type_die>();
			assert(t.is_real_die_position());
			auto concrete_t = t->get_concrete_type();
			pair<string, string> uniqtype_name_pair;
			string canonical_typename = dwarf::core::abstract_name_for_type(t);
			
			/* CIL/trumptr will only generate references to aliases in the case of 
			 * base types. We need to handle these here. What should happen? 
			 * 
			 * - we will see references looking like __uniqtype__signed_char
			 * - we want to link in two things:
			 *    1. the nameless __uniqtype_<code>_ definition of this base type
			 *    2. the alias    __uniqtype_<code>_signed_char from the usual alias handling
			 * - we do this by indexing all our types by a *codeless* version of their
			 *   name, then matching our inputs lines against that.
			 * - the input lines will have signed_char instead of ""
			 * - ... so that's what we need to put in our index.
			 * 
			 * IT GETS WORSE: the same is true for any typename *mentioning* a base
			 * type! We will see references in terms of C-canonicalised base type names, 
			 * but we will be trying to match them against language-independent names. 
			 * It seems that we need to do a separate "C fix up" pass first.
			 * This is now done in link-used-types (and will be 
			 * */
			
			
			if (canonical_typename == "")
			{
				assert(concrete_t.is_a<base_type_die>());
				// if the base type has no name, this DWARF type is useless to us
				if (!concrete_t.name_here()) continue;
				canonical_typename = *type_die_get_name(concrete_t);
			}
			string codeless_symname = mangle_typename(make_pair("", canonical_typename));

			m.insert(make_pair(codeless_symname, concrete_t));

			/* Special handling for base types: also add them by the name in which they 
			 * appear in the DWARF, *and* by their C-canonical name. Our CIL frontend
			 * doesn't know the exact bit-widths so must use the latter. */
			if (concrete_t.is_a<base_type_die>() && concrete_t.name_here()
				&& !concrete_t.as_a<base_type_die>()->is_bitfield_type())
			{
				m.insert(
					make_pair(
						mangle_typename(make_pair("", *type_die_get_name(concrete_t))), 
						concrete_t
					)
				);
				const char **equiv = abstract_c_compiler::get_equivalence_class_ptr(type_die_get_name(concrete_t)->c_str());
				if (equiv)
				{
					m.insert(
						make_pair(
							mangle_typename(make_pair("", equiv[0])), 
							concrete_t
						)
					);
				}
			}
		}
	}
}

} // end namespace tool
} // end namespace allocs
