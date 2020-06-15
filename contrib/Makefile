THIS_MAKEFILE := $(realpath $(lastword $(MAKEFILE_LIST)))
CONTRIB := $(realpath $(dir $(THIS_MAKEFILE)))

default: $(CONTRIB)/env.sh config.mk build-dwarfidl
include config.mk

subconfigs :=

.PHONY: clean default
# ----------------------------dwarfidl

dwarfidl/contrib/config.mk:
	cd dwarfidl && git submodule update --init --recursive
	cd dwarfidl && make -C contrib

dwarfidl/config.status: dwarfidl/contrib/config.mk
	cd dwarfidl && ./autogen.sh
	cd dwarfidl && . contrib/env.sh && ./configure --prefix=$(CONTRIB)/usr

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

config.mk: $(subconfigs)
	(printf "$(foreach cfg,$(subconfigs),\ninclude $(CONTRIB)/$(cfg))"; \
	printf '$(CONFIG_MK)'; echo) > "$@" || (rm -f "$@"; false)
# env.sh exports some vars
$(CONTRIB)/env.sh:
	(printf "$(foreach cfg,$(subconfigs),\nsource $$(dirname $(CONTRIB)/$(cfg))/env.sh)"; \
	printf '$(foreach var,$(varlist),\nexport $(var)="$($(var))")'; echo )>"$@" || (rm -f "$@"; false)

clean::
	rm -f config.mk env.sh
