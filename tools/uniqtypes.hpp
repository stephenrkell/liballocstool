#ifndef DUMPALLOCS_UNIQTYPES_HPP_
#define DUMPALLOCS_UNIQTYPES_HPP_

#include <sstream>
#include <fstream>
#include <memory>
#include <dwarfpp/lib.hpp>
#include <srk31/rotate.hpp>
#include <cstdint>
#include <iomanip>
#include <boost/optional.hpp>
#include "helpers.hpp"

// this encodes only the set of types, not the relations between them!
struct master_relation_t : public std::map< uniqued_name, dwarf::core::iterator_df<dwarf::core::type_die> >
{
	//using map::map;
	template<typename... Args>
	master_relation_t(Args&&... args): map(std::forward<Args>(args)...) {}

	map<dwarf::core::iterator_df<dwarf::core::type_die>, set< string > > aliases;
};

uniqued_name add_type(dwarf::core::iterator_df<dwarf::core::type_die> t, master_relation_t& r);
std::pair<bool, uniqued_name> add_type_if_absent(dwarf::core::iterator_df<dwarf::core::type_die> t, master_relation_t& r);
std::pair<bool, uniqued_name> add_concrete_type_if_absent(dwarf::core::iterator_df<dwarf::core::type_die> t, master_relation_t& r);
std::pair<bool, uniqued_name> transitively_add_type(dwarf::core::iterator_df<dwarf::core::type_die> t, master_relation_t& r);
void add_alias_if_absent(
	const std::string& s, 
	dwarf::core::iterator_df<dwarf::core::type_die> concrete_t, 
	master_relation_t& r
);

void make_exhaustive_master_relation(master_relation_t& r, 
	dwarf::core::iterator_df<> begin, 
	dwarf::core::iterator_df<> end);

void write_master_relation(master_relation_t& r, 
	std::ostream& out, std::ostream& err, bool emit_void, bool emit_struct_def, 
	std::set<std::string>& names_emitted,
	std::map<std::string, std::set< dwarf::core::iterator_df<dwarf::core::type_die> > >& types_by_name,
	bool emit_codeless_aliases,
	bool emit_subobject_names = true);

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
#endif
