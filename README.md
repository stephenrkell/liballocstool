liballocstool: a library for building tools that process allocation
metadata for binary programs

... where 'allocation metadata' is meant in the sense of the 'liballocs'
project. This metadata is basically an extension to debugging
information. It includes metadata about /allocation sites/ in code, and
a representation of /type information/. That information can be derived
from DWARF debugging information or other sources; the representation
here is suitable for in-memory use at run time. The code in this
repository can transform DWARF and allocation site info into C source,
which compiles down to such a representation convenient for loading at
run time.

This code was split from the liballocs tree because although liballocs
is the main client of this information, it is useful more widely. For
example, some of this metadata is also used in the 'sysfoot' system call
tracer / checker.
