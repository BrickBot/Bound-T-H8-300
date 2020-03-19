-- Flow.Unresolvable (decl)
--
-- Unresolvable dynamic flow and call edges, for use when the target
-- program contains a dynamic jump or call that we do not even try
-- to resolve.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:49 $
--
-- $Log: flow-unresolvable.ads,v $
-- Revision 1.2  2015/10/24 19:36:49  niklas
-- Moved to free licence.
--
-- Revision 1.1  2006-03-25 20:08:08  niklas
-- First version.
--


package Flow.Unresolvable is


   --
   --    Unresolvable jumps
   --


   type Edge_T is new Boundable_Edge_T with null record;
   --
   -- An unresolvable dynamic jump.


   type Edge_Ref is access all Edge_T'Class;
   --
   -- A reference to a heap-allocated unresolvable edge object.


   function Basis (Item : Edge_T) return Storage.Cell_List_T;
   --
   -- The basis of an unresolvable Edge_T is a null set.
   --
   -- Overrides (implements) Storage.Bounds.Basis.


   function Image (Item : Edge_T) return String;
   --
   -- Displays "Unresolvable".
   --
   -- Overrides Storage.Bounds.Image.


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in out Edge_T;
      Graph  : in     Graph_T);
   --
   -- Applies the Bounds Upon the boundable edge as explained for the
   -- abstract declaration Flow.Apply (Upon : Boundable_Edge_T).
   --
   -- The (default) implementation does nothing (leaving the edge
   -- Unresolved).
   --
   -- Overrides (implements) Flow.Apply (Upon : Boundable_Edge_T).


   function Edge return Dynamic_Edge_T;
   --
   -- A new unresovable edge.


   --
   --    Unresolvable calls
   --


   type Call_T is new Edge_T with null record;
   --
   -- An unresolvable dynamic call.
   --
   -- Inherits all operations from the unresolvable Edge_T.


   type Call_Ref is access all Call_T'Class;
   --
   -- A reference to a heap-allocated unresolvable call object.


   function Call return Dynamic_Edge_T;
   --
   -- A new unresovable call.


end Flow.Unresolvable;
