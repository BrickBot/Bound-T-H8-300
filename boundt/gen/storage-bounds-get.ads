-- Storage.Bounds.Get (decl)
--
-- Getting Storage.Bounds stuff from strings, similar to the
-- predefined 'Value attributes.
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-bounds-get.ads,v $
-- Revision 1.2  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013-02-03 12:37:05  niklas
-- First version.
--


package Storage.Bounds.Get is


   function Interval_Value (From : String)
   return Interval_T;
   --
   -- The interval denoted by the string, which can have the
   -- following forms:
   --
   --  Form            Meaning
   --  ----            -------
   --  ..              The universal interval    -inf .. +inf.
   --  value           The singleton interval    value .. value.
   --  value..         The half-bounded interval value .. +inf.
   --  ..value         The half-bounded interval -inf .. value.
   --  value1..value2  The bounded interval      value1 .. value2.
   --
   -- Leading, trailing, or embedded blanks are not allowed.
   -- Syntax errors cause Constraint_Error.


end Storage.Bounds.Get;
