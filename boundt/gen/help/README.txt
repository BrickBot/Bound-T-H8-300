The Bound-T "help" directory / directories

This is boundt/gen/help, the directory that contains the Bound-T "help"
files that Bound-T uses to describe the analysis options and their
values. This directory contains the descriptions of the generic,
target-independent options; other directories contain the descriptions
of target-specific or compiler-specific options.

Each option and each option group is described in its own file(s). For
some options each possible value of the option is also described in its
own file(s).

The descriptions are originally written in a dedicated XML form in files
with the suffix .xml, for example verbose.xml for the "-verbose" option.
The XML form is then translated to HTML and plain text by means of the
script help-xml.sh (which calls help-html.sh) and the XSLT transformer
to-html.xslt. These scripts can be found in the directory
boundt/tools/help.

Thus, each option (option group, option value) is described by three
files: the original XML file (verbose.xml); the translation into HTML
(verbose.html); and the translation into plain text (verbose.txt).
See the files template-option.xml, template-option-group.xml, and
template-option-value.xml.

The XML syntax is different for simple options, option groups, and
option values. Prefixed options use the same XML form as simple options.
There are specific XML forms for making references to other options,
prefixed options, and option values.

The files are named as follows:

- For simple options such as "-verbose", the file-name is just the
  option name, eg. "verbose.xml".

- For prefixed options such as "-trace decode", the file-name is
  the prefix, a period, and the option name, eg. "trace.decode.xml".

- For option groups, the file-name is the name of the group followed
  by "-group", eg. "imp-group.xml".

- For enumerated (symbolic literal) option values, the file-name is the
  name of the option, a comma, and the name of the value, eg.
  "show,model.xml" for the option "-show model".

The original XML files are stored (in CVS) under "boundt" as follows:

- boundt/gen/help, for general options not specific to a target processor
  or anything else.

- boundt/<target>/help, for target-specific options.

- boundt/lib/help, for options defined in "lib" modules. (Note: this means
  that no two lib modules can have options with the same names, although
  the lib modules are never used in the same Bound-T version.)

- boundt/opt/<optionalfunc>/help, for options specific to some optional
  Bound-T functionality.

At run-time, Bound-T searches for and uses help files from the a
directory tree defined by the environment variable BOUNDT_OPT, which
is planned to be:

- Under linux: /usr/local/share/doc/bound-t/help

- Under MS-Windows: TBD.

The following subdirectories under BOUNDT_HELP are searched for
help files (depending on the target and optional functionality
for the running version of Bound-T):

- BOUNDT_HELP/gen                 (all Bound-T versions)
- BOUNDT_HELP/lib                 (all Bound-T versions)
- BOUNDT_HELP/<target>            (per the current target)
- BOUNDT_HELP/opt/<optionalfunc>  (per the included optional funcs)

--
$Id: README.txt,v 1.3 2011-09-04 16:36:51 niklas Exp $
$Log: README.txt,v $
Revision 1.3  2011-09-04 16:36:51  niklas
Updated re run-time help directories.

Revision 1.2  2011-09-01 21:26:06  niklas
Editorial improvements.

Revision 1.1  2011-08-31 04:26:31  niklas
First version.

