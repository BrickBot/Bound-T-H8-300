-- Formats.Dwarf.Omni (decl)
--
-- Collecting all the DWARF debugging information.
--
-- The DWARF information has several parts of facets which are
-- implemented by dedicated child packages of Formats.Dwarf.
-- The present package, Formats.Dwarf.Omni, defines a data structure
-- to hold all the DWARF information, of course divided into sub-
-- structures for each facet.
--
-- Author: Niklas Holsti.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd, except for text copied verbatim
-- from the DWARF standard.
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
-- $Date: 2015/10/24 20:53:52 $
--
-- $Log: formats-dwarf-omni.ads,v $
-- Revision 1.2  2015/10/24 20:53:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2004-04-24 18:06:21  niklas
-- First version.
--


with Formats.Dwarf.Line_Numbers;


package Formats.Dwarf.Omni is


   --
   --    Collected DWARF information:
   --


   type Info_T is private;
   --
   -- A DWARF information set. It can hold, or access, all facets of
   -- debugging information, but all of them are optional and may be
   -- present or absent.
   --
   -- Attributes include:
   --
   -- > The format variant (Bits_T).
   --
   -- > Information for each DWARF facet; all optional.
   --
   -- To get DWARF information from an object or executable file the
   -- following steps are necessary:
   --
   -- 1. Create an Info_T object. It is initially empty.
   --
   -- 2. Open the object or executable file using Formats.IO.
   --    This is a task for the client; the Formats.Dwarf family does
   --    not help.
   --
   -- 3. Find one or more of the sections containing DWARF information.
   --    The names of the relevant sections are defined as constant
   --    strings in the Formats.Dwarf family. Otherwise, since the
   --    way to find sections differs between host formats such as ELF
   --    or COFF, finding DWARF sections is not a job for this family.
   --
   -- 4. Call the Load operation from the relevant Formats.Dwarf child
   --    package, passing it the location (IO index) of the relevant
   --    DWARF section in the file and the Info_T object. The Load
   --    operation attaches the DWARF information from the indicated
   --    segment to the Info_T object.
   --
   -- 5. Call on the services provided by the relevant Formats.Dwarf
   --    packages to use the DWARF information, passing the Info_T
   --    object and the query to be answered or executed.


   function Bits (Item : Info_T) return Bits_T;
   --
   -- The format variant (bits width).


   --
   --    Access operations:
   --


   

private

   type Info_Object_T;

   type Info_T is access Info_Object_T;

end Formats.Dwarf.Omni;
