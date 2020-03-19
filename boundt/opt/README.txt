OPTIONAL COMPONENTS OF THE BOUND-T TIMING ANALYSIS TOOL

Some Bound-T functionality is optional and is not included in every
release of the tool. The source code for such components is stored in
this directory, in two forms:

- A "null" form that does nothing but satisfies the
  compiler is stored directly in this "opt" directory.

- The real form that implements the functionality is
  stored in a subdirectory of "opt".

For example, an optional functionality represented by the procedure
World_Peace.Ensure would be represented by the following source-code
files:

opt/world_peace.ads              -- null (minimal) declaration
opt/world_peace.adb              -- null body
opt/world_peace/world_peace.ads  -- real declaration (if needed)
opt/world_peace/world_peace.adb  -- real body

The "real" subdirectory need not contain package declarations if the
"null" declarations are valid also for the "real" implementation.

The name of the subdirectory for the real functionality is not necessary
the same as that of any package in the functionality. In the above
example, the real source-code could be stored in a subdirectory called
opt/peace or opt/god.

This directory is intended to be used as follows:

- This "opt" directory is always put on the compiler's
  search path (ADA_INCLUDE for Gnat). Thus, if nothing else
  is done, the compiler always finds the "null" versions
  of the source-code files for all optional functionalities.

- To include an optional functionality, the relevant
  subdirectory of "opt" is put on the compiler's search
  path before the "opt" directory itself. Thus, the compiler
  finds the "real" versions of the source-code files
  for this functionality.

Note that for some optional functionalities, this directory, and its
subdirectories, may contain target-specific files related to that
functionality. In such cases, the subdirectory boundt/<target> does
not contain all <target>-specific code.

--
Copyright (c) 2006 Tidorum Ltd
$Revision: 1.2 $
