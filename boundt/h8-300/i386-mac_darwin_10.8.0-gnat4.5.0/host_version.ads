-- Host_Version (decl)
--
-- Identifies the host system on which the containing program is
-- compiled and linked. This code is generated automatically by
-- the script create-host-version.sh.
--
-- A component of the Bound-T time and space analyser. The information
-- in this package is displayed in response to the Bound-T command-line
-- option -host_version.


package Host_Version
is

   Summary : constant String := "i386-mac_darwin_10.8.0-gnat4.5.0";
   System  : constant String := "Darwin Kernel Version 12.5.0: Sun Sep 29 2013; x86_64";
   GNAT    : constant String := "4.5.0";

end Host_Version;
