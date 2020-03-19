-- File_System.UT01 (body)
--
-- A unit-test procedure for the Bound-T Worst-Case Execution Time Tool.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/25 07:28:15 $
--
-- $Log: file_system-ut01.adb,v $
-- Revision 1.2  2015/10/25 07:28:15  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-10-10 06:04:02  niklas
-- Moved here from "utest" because it is platform-specific.
--


with Unit_Test;
use  Unit_Test;


package body File_System.UT01 is


   procedure Test_Path_Name (
      Path   : in String;
      File   : in String;
      Result : in String)
   --
   -- Check that Path_Name (Path, File) = Result.
   --
   is
   begin

      Check (
         What   => "Path_Name(" & Path & ',' & File & ')',
         Expect => Result,
         Actual => Path_Name (Path, File));

   end Test_Path_Name;


   procedure Test_File_Name (
      Path   : in String;
      Result : in String)
   --
   -- Check that File_Name (Path) = Result.
   --
   is
   begin

      Check (
         What   => "File_Name(" & Path & ')',
         Expect => Result,
         Actual => File_Name (Path));

   end Test_File_Name;


   procedure Test_With_New_Suffix (
      Path   : in String;
      Suffix : in String;
      Result : in String)
   --
   -- Check that File_Name (Path) = Result.
   --
   is
   begin

      Check (
         What   => "With_New_Suffix(" & Path & ',' & Suffix & ')',
         Expect => Result,
         Actual => With_New_Suffix (Path, Suffix));

   end Test_With_New_Suffix;


   procedure Test_With_No_Suffix (
      Path   : in String;
      Result : in String)
   --
   -- Check that With_No_Suffix (Path) = Result.
   --
   is
   begin

      Check (
         What   => "With_No_Suffix(" & Path & ')',
         Expect => Result,
         Actual => With_No_Suffix (Path));

   end Test_With_No_Suffix;


   procedure Main
   is
   begin

      Begin_Test ("Path_Name");

      Test_Path_Name (
         Path   => "/usr/local",
         File   => "bin",
         Result => "/usr/local/bin");

      Test_Path_Name (
         Path   => "/usr/local/",
         File   => "bin",
         Result => "/usr/local/bin");

      End_Test;



      Begin_Test ("File_Name");

      Test_File_Name (
         Path   => "simple",
         Result => "simple");

      Test_File_Name (
         Path   => "main.c/",
         Result => "main.c");

      Test_File_Name (
         Path   => "some/relative/path/file.foo",
         Result => "file.foo");

      End_Test;


      Begin_Test ("With_New_Suffix");

      Test_With_New_Suffix (
         Path   => "simple",
         Suffix => "xch",
         Result => "simple.xch");

      Test_With_New_Suffix (
         Path   => "./simple",
         Suffix => "xch",
         Result => "./simple.xch");

      Test_With_New_Suffix (
         Path   => "main.c/",
         Suffix => "o",
         Result => "main.o");

      Test_With_New_Suffix (
         Path   => "some/relative/path/file.foo",
         Suffix => "scr",
         Result => "some/relative/path/file.scr");

      End_Test;


      Begin_Test ("With_No_Suffix");

      Test_With_No_Suffix (
         Path   => "simple",
         Result => "simple");

      Test_With_No_Suffix (
         Path   => "simple.xch",
         Result => "simple");

      Test_With_No_Suffix (
         Path   => "./simple.xch",
         Result => "./simple");

      Test_With_No_Suffix (
         Path   => "./simple",
         Result => "./simple");

      Test_With_No_Suffix (
         Path   => "main.c/",
         Result => "main");

      Test_With_No_Suffix (
         Path   => "some/relative/path/file.foo",
         Result => "some/relative/path/file");

      End_Test;


      Finish;

   end Main;


end File_System.UT01;
