#ifndef LIBALLOCSTOOL_UNIQTYPES_HPP_
#define LIBALLOCSTOOL_UNIQTYPES_HPP_

#include <sstream>
#include <fstream>
#include <memory>
#include <dwarfpp/lib.hpp>
#include <srk31/rotate.hpp>
#include <cstdint>
#include <iomanip>
#include <boost/optional.hpp>

namespace allocs
{
namespace tool
{

using std::string;
using std::pair;
using std::set;
using namespace dwarf;
using dwarf::spec::opt;
using dwarf::core::iterator_df;
using dwarf::core::type_die;
using dwarf::core::base_type_die;
using dwarf::core::root_die;

/* pair: <code, uniqtype name> */
typedef pair<string, string> uniqued_name;

// forward decl
inline uniqued_name
initial_key_for_type(iterator_df<type_die> t);

// this encodes only the set of types, not the relations between them!
struct master_relation_t : public std::map< uniqued_name, iterator_df<type_die> >
{
	//using map::map;
	template<typename... Args>
	master_relation_t(Args&&... args): map(std::forward<Args>(args)...) {}

	// from *actual* (not initial) key to a set of strings
	map<uniqued_name, set< string > > aliases;
	map<uniqued_name, uniqued_name> non_canonical_keys_by_initial_key;

	uniqued_name key_for_type(iterator_df<type_die> t)
	{
		auto initial_key = initial_key_for_type(t);
		auto found = non_canonical_keys_by_initial_key.find(initial_key);
		if (found != non_canonical_keys_by_initial_key.end())
		{
			return found->second;
		} else return initial_key;
	}
};

uniqued_name add_type(iterator_df<type_die> t, master_relation_t& r);
pair<bool, uniqued_name> add_type_if_absent(iterator_df<type_die> t, master_relation_t& r);
pair<bool, uniqued_name> add_concrete_type_if_absent(iterator_df<type_die> t, master_relation_t& r);
pair<bool, uniqued_name> transitively_add_type(iterator_df<type_die> t, master_relation_t& r);
void add_alias_if_absent(
	const std::string& s, 
	iterator_df<type_die> concrete_t, 
	master_relation_t& r
);
void emit_extern_declaration(std::ostream& out,
	const uniqued_name& name_pair,
	bool force_weak);
void emit_weak_alias_idem(std::ostream& out,
	const string& alias_name, const string& target_name, bool emit_section = true );
void make_exhaustive_master_relation(master_relation_t& r, 
	iterator_df<> begin, 
	iterator_df<> end);

extern const char *pervasives_raw_names[];

void write_master_relation(master_relation_t& r, 
	std::ostream& out, std::ostream& err,
	std::set<std::string>& names_emitted,
	std::map<std::string, std::set< iterator_df<type_die> > >& types_by_name,
	bool emit_codeless_aliases,
	bool emit_subobject_names = true);

void write_uniqtype_section_decl(std::ostream &o, const string& mangled_typename);

void write_uniqtype_open_void(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    boost::optional<string> comment_str = boost::optional<string>()
	);
void write_uniqtype_open_array(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
    unsigned nelems,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_flex_array(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_address(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
    unsigned indir_level,
    bool is_generic,
    unsigned log_min_align,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_base(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
    unsigned enc,
    unsigned one_plus_log_bit_size_delta,
    signed bit_size_delta_delta,
    signed bit_off,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_subrange(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
	signed min,
	signed max,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_enumeration(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_composite(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
    unsigned nmemb,
    bool not_simultaneous,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);
void write_uniqtype_open_subprogram(std::ostream& o,
    const string& mangled_typename,
    const string& unmangled_typename,
    unsigned pos_maxoff,
    unsigned narg,
    unsigned nret,
    bool is_va,
    unsigned cc,
    boost::optional<string> comment_str = boost::optional<string>(),
	bool use_section_group = true,
	bool make_weak_definition = false
	);

void write_uniqtype_related_array_element_type(std::ostream& o,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );
void write_uniqtype_related_pointee_type(std::ostream& o,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );
void write_uniqtype_related_ultimate_pointee_type(std::ostream& o,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );
void write_uniqtype_related_subprogram_argument_type(std::ostream& o,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );
void write_uniqtype_related_subprogram_return_type(std::ostream& o,
	bool is_first,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );
void write_uniqtype_related_contained_member_type(std::ostream& o,
    bool is_first,
	unsigned offset,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );

void write_uniqtype_related_member_names(std::ostream& o,
	bool is_first,
	optional<string> maybe_subobj_names = boost::optional<string>(),
	optional<string> comment_str = boost::optional<string>()
	);
void write_uniqtype_related_signedness_complement_type(std::ostream& o,
    boost::optional<string> maybe_mangled_typename = boost::optional<string>(),
	boost::optional<string> comment_str = boost::optional<string>()
    );
void write_uniqtype_related_dummy(std::ostream& o,
	boost::optional<string> comment_str = boost::optional<string>()
    );
	
void write_uniqtype_close(std::ostream& o,
	const string& mangled_name,
	boost::optional<unsigned> n_contained = boost::optional<unsigned>());

int dump_usedtypes(const std::vector<std::string>& fnames, std::ostream& out, std::ostream& cerr);


string
codestring_for_type(iterator_df<type_die> t);
inline uniqued_name
initial_key_for_type(iterator_df<type_die> t)
{
	return make_pair(codestring_for_type(t),
		/*(t && t->get_concrete_type())*/ dwarf::core::abstract_name_for_type(t));
}

string 
name_for_complement_base_type(iterator_df<base_type_die> base_t);

string 
summary_code_to_string(opt<uint32_t> code);

// iterator_df<type_die>
// find_type_in_cu(iterator_df<compile_unit_die> cu, const string& name);

inline string mangle_spaces(const string& s)
{
	string mangled = s ;
	replace(mangled.begin(), mangled.end(), ' ', '_');

	return mangled;
}

inline string mangle_string(const string& s)
{
	string mangled = s;
	static locale_t c_locale = newlocale(LC_CTYPE_MASK, "C", NULL);
	assert(c_locale);
	auto i_char = mangled.begin();
	while (i_char != mangled.end())
	{
		char c = *i_char;
		unsigned pos = i_char - mangled.begin();
		if (!isalnum_l(c, c_locale) && c != '_' && c != '$' && c != '\0')
		{
			std::ostringstream s;
			s << "$" << std::hex << std::setw(2) << std::setfill('0') << (int) c;
			mangled.replace(pos, 1, s.str());
			i_char = mangled.begin() + pos + 3;
		}
		else if (c == '$')
		{
			mangled.replace(pos, 1, "$$");
			i_char = mangled.begin() + pos + 2;
		} else ++i_char;
	}
	return mangled;
}

inline string mangle_typename(const pair<string, string>& p)
{
	return "__uniqtype_" + p.first + "_" + mangle_string(p.second);
}

opt<uint32_t> type_summary_code(iterator_df<type_die> t);
opt<uint32_t> signedness_complement_type_summary_code(iterator_df<base_type_die> base_t);

inline std::string offset_to_string(lib::Dwarf_Off o)
{
	std::ostringstream s;
	s << "0x" << std::hex << o << std::dec;
	return s.str();
}

void get_types_by_codeless_uniqtype_name(
	std::multimap<string, iterator_df<type_die> >& types_by_codeless_uniqtype_name, 
	iterator_df<> begin, iterator_df<> end);

iterator_df<type_die> get_or_create_uninterpreted_byte_type(root_die& r);
#define IS_VARIADIC(t) \
((t).is_a<dwarf::core::subroutine_type_die>() ? (t).as_a<dwarf::core::subroutine_type_die>()->is_variadic() \
		:   (t).is_a<dwarf::core::subprogram_die>() ? (t).as_a<dwarf::core::subprogram_die>()->is_variadic() \
		:   false )
#define RETURN_TYPE(t) \
((t).is_a<dwarf::core::subroutine_type_die>() ? (t).as_a<dwarf::core::subroutine_type_die>()->get_type() \
		:   (t).is_a<dwarf::core::subprogram_die>() ? (t).as_a<dwarf::core::subprogram_die>()->get_type() \
		:   (assert(false), iterator_base::END) )

inline opt<string>
name_for_type_die(iterator_df<type_die> t)
{
	/* Normally we just return the name. However: HACK HACK HACK. 
	 * If it's a CIL name like __anon(struct|union)_BLAH_nn, we erase the nn. 
	 * This is so that we don't generate nominally distinct types 
	 * in different compilation units. */
	/*if (t.name_here() && (t.name_here()->find("__anonstruct_") == 0
					|| t.name_here()->find("__anonunion_") == 0
					|| t.name_here()->find("__anonenum_") == 0))
	{
		string replacement_name = *t.name_here();
		unsigned last_underscore_pos = replacement_name.find_last_of('_');
		assert(last_underscore_pos && last_underscore_pos + 1 < replacement_name.length());
		replacement_name.replace(last_underscore_pos, 
			replacement_name.length() - last_underscore_pos, "_1");
		return replacement_name;
	}
	else*/ if (t.is_a<core::subprogram_die>())
	{
		/* When interpreted as types, subprograms don't have names. */
		return opt<string>();
	}
	else return *t.name_here();
}

} // end namespace tool
} // end namespace allocs

#endif
