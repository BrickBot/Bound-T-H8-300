-- File_System (decl) for MS-Windows.
--
-- Platform-dependent file-system properties and functions.
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
-- $Revision: 1.4 $
-- $Date: 2015/10/25 07:30:55 $
--
-- $Log: file_system.ads,v $
-- Revision 1.4  2015/10/25 07:30:55  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-10-10 05:59:38  niklas
-- Extended function Path_Name to remove trailing path
-- separators from the Path before adding the File name.
-- Added functions With_New_Suffix, With_No_Suffix, and
-- Is_Readable_Text.
--
-- Revision 1.2  2006/05/08 16:52:18  Niklas
-- Added function File_Name.
--
-- Revision 1.1  2005/09/20 19:35:18  niklas
-- First version, for BT-CH-0011.
--


package File_System is


   function Path_Name (
      Path : String;
      File : String)
   return String;
   --
   -- Combines a Path name (which may be simply the name of a
   -- directory) and a File name into a string that names this
   -- File on this Path (or in this directory).
   --
   -- On typical systems this simply concatenates the Path, the
   -- system-specific path-name separator (e.g '\' on MS-Win) and
   -- the File. However, we first remove any trailing path-name
   -- separators from the Path so that the File name is preceded
   -- by only one separator.


   function File_Name (Path : String) return String;
   --
   -- The basic file-name from the Path, stripping all drive and
   -- directory names (prefixes). Any trailing path-delimiters
   -- in the Path are ignored (removed). However, the suffix
   -- ("file type") if any is not removed.


   function With_New_Suffix (
      Path   : String;
      Suffix : String)
   return String;
   --
   -- The Path, with its suffix (the part of the file-name that comes
   -- after the last '.', if any) replaced by the given Suffix.
   -- If the Path has no suffix then a '.' and the new Suffix are
   -- simply added at the end of the Path.
   --
   -- Any trailing path-delimiters in the Path are ignored (removed).
   -- If that leaves an empty string the function returns '.' & Suffix.


   function With_No_Suffix (Path : String)
   return String;
   --
   -- The Path, without its suffix (the part of the file-name that
   -- comes after the last '.', if any). The '.' is also removed.
   -- If the Path has no suffix then the Path is returned.
   -- Any trailing path-delimiters in the Path are ignored (removed)
   -- in both cases.
   --
   -- Note that the result is not the same as With_New_Suffix (Path, "")
   -- because With_New_Suffix includes a trailing '.'.


   function Is_Readable_Text (Path : String) return Boolean;
   --
   -- Whether the file named by the given Path exists and can
   -- be read as text, that is, can be opened by Ada.Text_IO
   -- for input. However, the answer is only temporary, of course;
   -- even if the answer is True the file can be removed or
   -- made unreadable at any time, by some other process.


end File_System;
