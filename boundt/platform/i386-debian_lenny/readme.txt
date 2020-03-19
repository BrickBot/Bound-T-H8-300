README for boundt/platform/i386-debian_lenny

This directory contains third-party object libraries for linking
into the Bound-T time and stack analysis tool, when building
the tool for the Intel-386 / Debian-Lenny host platform.

The libraries are not necessarily restricted to exactly these
versions of the host CPU or host O/S, but may be more widely
usable. However, for clarity we provide a separate directory for
each Bound-T build host system, with the consequence that the
same object libraries may be present in several such directories.

Please see the CVS comments for identification of each file
in terms of the name and version as used by the third-party
provider.

When necessary, the host-system-specific versions of the source
code of the Ada bindings to these libraries are also stored here.
These Ada files typically contain only type declarations, subprogram
declarations, and Import pragmas.

--
$Id: readme.txt,v 1.1 2010-12-06 17:17:55 niklas Exp $
