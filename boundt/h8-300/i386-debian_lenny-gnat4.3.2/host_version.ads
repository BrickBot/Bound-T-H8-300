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

   Summary : constant String := "i386-debian_lenny-gnat4.3.2";
   System  : constant String := "Linux compax 2.6.26-2-686 #1 SMP Thu Nov 25 01:53:57 UTC 2010 i686 GNU/Linux";
   GNAT    : constant String := "4.3.2";

end Host_Version;
