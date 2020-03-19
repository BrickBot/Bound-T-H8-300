-- Storage.Reserved_Names
--
-- Reserved (ie. forbidden) names of cells.
--
-- Cell names are used in some contexts (eg. Calculator.Formulas) in
-- which they must not be confused with the names of other, cell-like
-- things (eg. synthetic iteration counter identifiers). Here we list
-- all the reserved names. When a new cell is created, its Name is
-- checked against these reserved names, and a Fault is emitted if
-- there is a conflict.
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-reserved_names.ads,v $
-- Revision 1.3  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.2  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.1  2008/04/26 19:19:45  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--


package Storage.Reserved_Names is


   type Item_T is (Iter_Count, Mod_Factor1, Mod_Factor2);
   --
   -- Listing all the reserved names.
   --
   -- Iter_Count
   --    The identifier for a synthetic, joint loop-iteration
   --    counter, used in Calculator.Formulas for modelling the
   --    values of induction variables.
   -- Mod_Factor1, 2
   --   The identifieds for existentially quantified variables
   --   used to compute "mod 2**W" when modelling comparisons of
   --   W-bit numbers using modular arithmetic.


   type Name_Ref is access String;
   --
   -- Refers to a reserved name.


   Name : constant array (Item_T) of Name_Ref := (
      Iter_Count  => new String'("n_"),
      Mod_Factor1 => new String'("m1_"),
      Mod_Factor2 => new String'("m2_"));
   --
   -- All the reserved names.
 

end Storage.Reserved_Names;
