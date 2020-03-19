-- Bound_T.Opt (body)
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.3 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: bound_t-opt.adb,v $
-- Revision 1.3  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-09-01 22:15:42  niklas
-- Added Show_Licence(_Opt), registered as "-licence" and "-license".
-- Added synonym "-synonyms" for "-synonym".
--
-- Revision 1.1  2011-08-31 04:17:12  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


with Options.Groups;


package body Bound_T.Opt is


   function Time_Image (Item : Duration) return String
   is
   begin

      if Item = Time_Unlimited then

         return "unlimited";

      else

         return Duration'Image (Item);

      end if;

   end Time_Image;


   HRT_Group : constant Options.Group_Name_T := Options.Group ("hrt");
   --
   -- The group of options related to Hard-Real-Time modelling.


begin

   Options.Register (
      Option => Show_Version_Opt'access,
      Name   => "version",
      Group  => Options.Groups.Outputs);

   Options.Register (
      Option => Show_Host_Version_Opt'access,
      Name   => "host_version",
      Group  => Options.Groups.Outputs);

   Options.Register (
      Option  => Show_Licence_Opt'access,
      Name    => "licence",
      Synonym => "license",
      Group   => Options.Groups.Outputs);

   Options.Register (
      Option => Dump_Program_Opt'access,
      Name   => "dump",
      Group  => Options.Groups.Outputs);

   Options.Register (
      Option => HRT_Opt'access,
      Name   => "hrt",
      Groups => (Options.Groups.Inputs, HRT_Group));

   Options.Register (
      Option => Time_Analysis_Opt'access,
      Name   => "anatime",
      Group  => Options.Groups.Outputs);

   Options.Register (
      Option => Max_Analysis_Time_Opt'access,
      Name   => "max_anatime",
      Group  => Options.Groups.Resource_Limits);

   Options.Register (
      Option  => List_Synonyms_Opt'access,
      Name    => "synonym",
      Synonym => "synonyms",
      Group   => Options.Groups.Outputs);

   Options.Register (
      Option => Deallocate_Opt'access,
      Name   => Options.Imp_Item ("dealloc"),
      Groups => (Options.Groups.Imp, Options.Groups.Host_Memory));

end Bound_T.Opt;
