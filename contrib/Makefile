THIS_MAKEFILE := $(realpath $(lastword $(MAKEFILE_LIST)))
CONTRIB := $(realpath $(dir $(THIS_MAKEFILE)))

default: $(CONTRIB)/env.sh config.mk build-dwarfidl
include config.mk

subconfigs :=

.PHONY: clean default
# ----------------------------dwarfidl

dwarfidl/contrib/config.mk dwarfidl/contrib/env.sh:
	cd dwarfidl && $(MAKE) -C contrib config.mk env.sh

dwarfidl/configure: dwarfidl/configure.ac
	cd dwarfidl && ./autogen.sh

dwarfidl/config.status: dwarfidl/configure dwarfidl/contrib/config.mk dwarfidl/contrib/env.sh
	cd dwarfidl && $(MAKE) -C contrib && . contrib/env.sh && ./configure

.PHONY: build-dwarfidl
build-dwarfidl: dwarfidl/config.status
	$(MAKE) -C dwarfidl

clean::
	if test -e dwarfidl/Makefile; then $(MAKE) -C dwarfidl clean; fi

CONFIG_MK += \nDWARFIDL_CXXFLAGS += -I$(CONTRIB)/dwarfidl/include
CONFIG_MK += \nDWARFIDL_LIBS += -L$(CONTRIB)/dwarfidl/lib -Wl,-rpath,$(CONTRIB)/dwarfidl/lib -ldwarfidl
varlist += DWARFIDL_CXXFLAGS DWARFIDL_LIBS
subconfigs += dwarfidl/contrib/config.mk

# ----------------------------config.mk
# If the user nukes a sub-config.mk that our config.mk depends on,
# we'd like to be able to rebuild it. So we include a generic rule
# for rebuilding config.mks.
config.mk: $(subconfigs)
	(printf '%%/config.mk:\n\t$$(MAKE) -C $$* config.mk\n\n'; \
	printf "$(foreach cfg,$(subconfigs),\ninclude $(CONTRIB)/$(cfg))"; \
	printf '$(CONFIG_MK)'; echo) > "$@" || (rm -f "$@"; false)
# env.sh exports some vars
env.sh $(CONTRIB)/env.sh: $(foreach cfg,$(subconfigs),$(dir $(CONTRIB)/$(cfg))env.sh)
	(printf "$(foreach cfg,$(subconfigs),\n. $$(dirname $(CONTRIB)/$(cfg))/env.sh)"; \
	printf '$(foreach var,$(varlist),\nexport $(var)="$($(var))")'; echo )>"$@" || (rm -f "$@"; false)
# Similarly when we make an env.sh, have Make ensure existence of
# its depended-on env.sh too, so that we don't get 'source'-time
# failures of sub-env.sh.
$(foreach cfg,$(subconfigs),$(dir $(CONTRIB)/$(cfg))env.sh): %/env.sh:
	$(MAKE) -C $* env.sh
clean::
	rm -f config.mk env.sh
