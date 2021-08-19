ACLOCAL_AMFLAGS = -I m4

SUBDIRS = . lib

# HACK to disable -O2 if DEBUG is set (to anything) -- thanks for nothing, automake
AM_CXXFLAGS = -O2 $(DEBUG:%=-O0) -std=c++14 -ggdb -fkeep-inline-functions -Wall \
 -Wno-deprecated-declarations -fPIC \
 -Wp,-w -Iinclude $(LIBSRK31CXX_CFLAGS) $(LIBCXXFILENO_CFLAGS) \
 $(LIBDWARFPP_CFLAGS) $(LIBCXXGEN_CFLAGS) $(LIBANTLR3CXX_CFLAGS) \
 $(DWARFIDL_CFLAGS) -I$(LIBRUNT)/include

AM_CPPFLAGS =
AM_CFLAGS = -Wall -std=gnu99 -ggdb -O3 -flto -Iinclude -fPIC -Wp,-w
AM_LDFLAGS =

# ltdl complains if I don't set these....
noinst_LTLIBRARIES =
include_HEADERS =
lib_LTLIBRARIES =
EXTRA_DIST =
EXTRA_LTLIBRARIES =
CLEANFILES =
MOSTLYCLEANFILES =

extra_DIST = liballocstool.pc.in
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = liballocstool.pc

liballocstool_includedir = $(includedir)/liballocstool
liballocstool_include_HEADERS = include/uniqtype-defs.h include/uniqtype.h include/allocmeta-defs.h

lib_LIBRARIES = src/liballocstool.a

src_liballocstool_a_SOURCES = src/stickyroot.cpp src/uniqtypes.cpp src/allocsites-info.cpp

LIBELF ?= -lelf
export LIBELF

# pkg-config doesn't understand PKG_CXXFLAGS, but I'm buggered
# if I'm going to have my Makefiles use _CFLAGS to mean _CXXFLAGS.
# So, if we find we have _CFLAGS set for these, either from pkg.m4
# or because pkg.m4 told the user to set this var, and if we *don't*
# have _CXXFLAGS for these things, propagate. Only one of the USER_*_CFLAGS
# and *_CFLAGS should be non-empty. Note that if we got this far,
# we *must* have a _CFLAGS, even if the user (sanely) didn't call it that,
# because pkg-config will have complained if we didn't fake it up.
if SUPPLIED_LIBCXXFILENO_CFLAGS
LIBCXXFILENO_CXXFLAGS = $(USER_LIBCXXFILENO_CFLAGS)
else
LIBCXXFILENO_CXXFLAGS = $(LIBCXXFILENO_CFLAGS)
endif
if SUPPLIED_LIBDWARFPP_CFLAGS
LIBDWARFPP_CXXFLAGS = $(USER_LIBDWARFPP_CFLAGS)
else
LIBDWARFPP_CXXFLAGS = $(LIBDWARFPP_CFLAGS)
endif
if SUPPLIED_LIBCXXGEN_CFLAGS
LIBCXXGEN_CXXFLAGS = $(USER_LIBCXXGEN_CFLAGS)
else
LIBCXXGEN_CXXFLAGS = $(LIBCXXGEN_CFLAGS)
endif
if SUPPLIED_LIBSRK31CXX_CFLAGS
LIBSRK31CXX_CXXFLAGS = $(USER_LIBSRK31CXX_CFLAGS)
else
LIBSRK31CXX_CXXFLAGS = $(LIBSRK31CXX_CFLAGS)
endif
if SUPPLIED_LIBANTLR3CXX_CFLAGS
LIBANTLR3CXX_CXXFLAGS = $(USER_LIBANTLR3CXX_CFLAGS)
else
LIBANTLR3CXX_CXXFLAGS = $(LIBANTLR3CXX_CFLAGS)
endif
if SUPPLIED_DWARFIDL_CFLAGS
DWARFIDL_CXXFLAGS = $(USER_DWARFIDL_CFLAGS)
else
DWARFIDL_CXXFLAGS = $(DWARFIDL_CFLAGS)
endif

LIBRUNT = $(librunt)

export LIBCXXFILENO_CXXFLAGS LIBCXXFILENO_LIBS \
LIBSRK31CXX_CXXFLAGS LIBSRK31CXX_LIBS \
LIBDWARFPP_CXXFLAGS LIBDWARFPP_LIBS \
LIBCXXGEN_CXXFLAGS LIBCXXGEN_LIBS \
LIBANTLR3CXX_CXXFLAGS LIBANTLR3CXX_LIBS \
DWARFIDL_CXXFLAGS DWARFIDL_LIBS \
CC CXX CPP CXXFLAGS LDFLAGS LDLIBS

if DEBUG
export DEBUG = 1
endif
