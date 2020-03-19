-- Stdio (decl)
--
-- Interface to the Unix/Linux/C "stdio" library.
-- This is a very limited view of the library, containing just
-- those routines that we have needed so far.
--
-- Author: Niklas Holsti.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: stdio.ads,v $
-- Revision 1.2  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.1  2005-03-08 10:19:13  niklas
-- First version.
--


with Interfaces.C.Strings;
with System;


package Stdio is


   use Interfaces.C;
   use Interfaces.C.Strings;


   type File_Ref is new System.Address;
   --
   -- A reference to a FILE.


   function Is_Null (Item : File_Ref) return Boolean;
   --
   -- Whether the Item is a null reference.


   Std_In, Std_Out, Std_Err : File_Ref;
   --
   -- The three standard I/O channels as File_Refs.
   --
   pragma Import (C, Std_In , External_Name => "stdin");
   pragma Import (C, Std_Out, External_Name => "stdout");
   pragma Import (C, Std_Err, External_Name => "stderr");


   function Fopen (
      Path : in chars_ptr;
      Mode : in chars_ptr)
   return File_Ref;
   --
   pragma Import (C, Fopen, External_Name => "fopen");


   function Fclose (File : File_Ref) return int;
   --
   pragma Import (C, Fclose, External_Name => "fclose");


   procedure Open_File (
      Path : in     String;
      Mode : in     String;
      File :    out File_Ref);
   --
   -- Opens the File, in the given Mode, with the given Path name.
   -- Returns File as a null reference (Is_Null (File)) if there is
   -- an error.


   procedure Close_File (File : in File_Ref);
   --
   -- Closes the File, natch.


end Stdio;
