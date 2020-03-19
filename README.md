Bound-T
=======
A software tool that uses static analysis of machine code to
compute upper bounds on the execution time (WCET) and stack
usage of embedded programs.


This repository contains the source code for the Bound-T static
analysis tool, version 4b, for the target processor h8-300.

Original Project Website – http://www.bound-t.com/

Renesas H8/300 Specifics – http://www.bound-t.com/targets/h8-300/

This top-level folder contains the following sub-folders:

- boundt, the source code of Bound-T itself

- OpenToken, the source code of the OpenToken library for
  lexical analysis and parsing. Bound-T uses this library for
  reading "assertion" files and for other similar purposes.

- lgl-adt, the source code of a library for abstract data types,
  including containers.

- mw_components, another library of Ada components.

- possibly (for some target processors) further libraries.


Additionally, Bound-T relies on the open-source programs oc and
lp_solve for parts of its analysis. All present versions of
Bound-T, for all target processors and on all host systems, use
the same versions of these support programs. For this archive for
the target processor h8-300, these programs have also been
included among the the following top-level sub-folders:

 - lp_solve

 - omega2


For building Bound-T you need a command shell (preferably bash)
and an Ada compiler (typically GNAT). For Microsoft Windows
hosts, we suggest that you use the MinGW port of bash, gcc, and
GNAT, from www.mingw.org.

To build Bound-T, go to the folder boundt/h8-300, and then to
the sub-folder for your host computer and OS, for example
i386-mac_darwin-gnat4.5.0. If there is no sub-folder that exactly
matches your host computer, try the closest match. The last
part of the folder name - the version of GNAT -- is hardly ever
significant. In fact, the build scripts for all Unix/Linux hosts
are usually identical, as are the build scripts for all Microsoft
Windows hosts.

In the chosen sub-folder of boundt/h8-300, there is a shell script
called build-open-source.sh. Execute this script in the chosen
sub-folder. This will use the GNAT Ada compiler to build Bound-T.
The result should be two executable files in this sub-folder:
boundt_h8-300, which is the dynamically linked executable, and
boundt_h8-300.static, which has the GNAT libraries statically
linked and can therefore be executed on hosts where GNAT is not
installed. On some host systems these two executables are
essentially identical, and the ".static" form may not exist.


If you have any problems building Bound-T, feel free to ask for help
by e-mail to info [at] tidorum.fi.
