THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
SRCROOT := $(realpath $(dir $(THIS_MAKEFILE))/..)

.PHONY: all
all: liballocstool.a

liballocstool.a:
	ln -sf "$(SRCROOT)"/src/liballocstool.a $@

clean:
	rm -f liballoctool.a
