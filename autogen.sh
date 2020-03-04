#!/bin/sh
aclocal && \
autoconf && \
libtoolize --force && \
autoheader && \
automake --add-missing && \
automake

# autoreconf --force --install -I config -I m4 && \
