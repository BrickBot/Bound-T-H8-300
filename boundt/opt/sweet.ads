-- SWEET (decl)
--
-- Interfaces between Bound-T and the SWEET value-and-flow-analysis tool from
-- Mälardalen Centre for Real-Time Systems. These interfaces let Bound-T
-- use SWEET to help with various analysis of values and execution flows.
--
-- This is the dummy/null version of this package, for use when the
-- SWEET interface is omitted.
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 21:12:00 $
--
-- $Log: sweet.ads,v $
-- Revision 1.4  2015/10/24 21:12:00  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.3  2013/12/23 21:17:19  niklas
-- Include Finish_Options and assertion sets.
--
-- Revision 1.2  2013-11-29 10:40:23  niklas
-- Updated to match sweet/sweet.ads.
--
-- Revision 1.1  2013-02-03 21:07:29  niklas
-- BT-CH-0239: SWEET for dynamic flow analysis - step 1.
--


with Assertions;
with Programs;
with Programs.Execution;


package SWEET is


   Included : constant Boolean := False;
   --
   -- Whether the SWEET option is included in the running Bound-T.

   -- TBA application interfaces. (The mechanism interfaces are
   -- defined only in and for the implementation and therefore not
   -- present in this dummy version.)


   procedure Finish_Options (Took_Action : in out Boolean);
   --
   -- Called after parsing and handling all "command-line" options
   -- given to Bound-T. May generate some output, for example showing
   -- the current version of SWEET, or whether SWEET is included in
   -- this Bound-T version at all. Therefore, this procedure is given
   -- a body even in this null version of SWEET.
   --
   -- Sets Took_Action to True if some positive, user-visible action
   -- was taken, such as showing the current SWEET version.


   procedure Resolve_Dynamic_Jumps (
      Subprogram : in Programs.Subprogram_T;
      Bounds     : in Programs.Execution.Bounds_Ref;
      Num_Bounds : in Programs.Execution.Bounds_Count_T;
      Asserts    : in Assertions.Assertion_Set_T)
   is null;


end SWEET;
