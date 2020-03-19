-- Storage.References (decl)
--
-- Boundable (dynamic) references to storage cells and areas.
-- Such references are one form of "boundable" object that can be
-- resolved by applying data "bounds". Here, the result of resolving
-- a reference is a storage cell (or TBA a set or range of cells).
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
-- 
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
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-references.ads,v $
-- Revision 1.4  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.2  2007/10/04 19:48:51  niklas
-- BT-CH-0083: Resolving dynamic access to constant data.
--
-- Revision 1.1  2005/02/16 21:11:49  niklas
-- BT-CH-0002.
--


with Arithmetic_Base;
with Storage.Bounds;


package Storage.References is


   type Boundable_T is abstract new Storage.Bounds.Boundable_T
   with record
      Width : Arithmetic_Base.Width_T;
   end record;
   --
   -- A reference (pointer) to a storage cell) with a dynamically computed
   -- identity (address), where the final identity can be bounded (perhaps
   -- to a single value) if we have bounds on the cells used in the
   -- computation of the identity. The Width of the referenced cell is
   -- known, although the its address is not known (yet).
   --
   -- The Width must generally be set when the reference is created, and
   -- not changed thereafter (after the reference becomes part of an
   -- arithmetic expression).


   type Boundable_Ref is access all Boundable_T'Class;
   --
   -- A reference to a heap-allocated boundable reference object.


   function Alias_Range (Ref : Boundable_T)
   return Alias_Range_T
   is abstract;
   --
   -- The alias range that can be reached by the Ref, with no
   -- known bounds on cell values. This range is usually defined by
   -- the kind of dynamic access that the Ref represents, for
   -- example which kind of memory (program, data, internal, external,
   -- i/o) it can access.


   function Alias_Range (
      Ref   : Boundable_T;
      Under : Bounds.Bounds_T'Class)
   return Alias_Range_T
   is abstract;
   --
   -- The alias range that can be reached by the Ref, Under some
   -- bounds on cell values. This must be a subset of the Alias_Range
   -- of the Ref when no bounds are known.


   function Referent (
      Ref   : Boundable_T;
      Under : Bounds.Bounds_T'Class)
   return Cell_T
   is abstract;
   --
   -- The single cell to which the boundable Ref can refer, Under
   -- the given bounds. (Representation of cell ranges, eg arrays, TBA.)
   --
   -- If a single referent cell is returned, this cell must be in the
   -- Alias_Range of the Ref when Under these bounds.
   --
   -- If the bounds do not constrain the Ref to a single referenced
   -- cell, the function shall return No_Cell.


   procedure Check_Constancy (
      Cell  : in     Cell_T;
      Ref   : in out Boundable_T;
      Const :    out Boolean;
      Value :    out Word_T);
   --
   -- Checks if the Cell, which is a Referent of the Ref, is
   -- a Constant cell and if so returns the constant Value of
   -- the Cell. If the Value is unknown the Cell should not be
   -- classified as a Constant cell.
   --
   -- The default implementation considers all Cells to be
   -- variable (not constant).


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Boundable_T;
      Giving :    out Boundable_Ref)
   is abstract;
   --
   -- Applies the given Bounds Upon a boundable reference (that was not
   -- resolved to a single Referent cell), perhaps Giving a more
   -- constrained boundable reference (one with a smaller possible range).
   -- If a more constrained reference does not result, the procedure shall
   -- return Giving as null.
   --
   -- This operation does not override Storage.Bounds.Apply because the
   -- parameter profile is different.


end Storage.References;
