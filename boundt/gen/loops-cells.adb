-- Loops.Cells (body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: loops-cells.adb,v $
-- Revision 1.4  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2007-10-05 20:33:43  niklas
-- BT-CH-0086: Use also primitive model for loop "uses/defines".
-- BT-CH-0086: Use optional "no_" prefix for more options.
--
-- Revision 1.2  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.1  2004/04/28 19:35:35  niklas
-- First version.
--


with Loops.Cells.Opt;


package body Loops.Cells is


   function Nodes (
      Within : Loop_T;
      Model  : Flow.Computation.Model_Ref)
   return Flow.Node_List_T
   --
   -- All the nodes Within the given loop.
   -- The computation Model provides access to the flow-graph.
   --
   is
   begin

      return
         Flow.To_List (
            Set  => Members (Within).all,
            From => Flow.Computation.Graph (Model));

   end Nodes;


   function Used_By (
      Luup  : Loop_T;
      Under : Flow.Computation.Model_Ref)
   return Storage.Cell_List_T
   is
   begin

      return
         Flow.Computation.Cells_Used (
            Nodes => Nodes (Luup, Under),
            Under => Under);

   end Used_By;


   function Defined_By (
      Luup  : Loop_T;
      Under : Flow.Computation.Model_Ref)
   return Storage.Cell_List_T
   is
   begin

      return
         Flow.Computation.Cells_Defined (
            Nodes => Nodes (Luup, Under),
            Under => Under);

   end Defined_By;


   function Is_Used (
      Location : Storage.Location_T;
      By       : Loop_T;
      Under    : Flow.Computation.Model_Ref)
   return Boolean
   is

      Loop_Nodes : constant Flow.Node_List_T := Nodes (By, Under);
      -- The nodes in the list.

   begin

      return
         Flow.Computation.Is_Used (
            Location => Location,
            By       => Loop_Nodes,
            Under    => Under)

      or else (
         Opt.Include_Primitive
      and then
         Flow.Is_Used (
            Location => Location,
            By       => Loop_Nodes));

   end Is_Used;


   function Is_Defined (
      Location : Storage.Location_T;
      By       : Loop_T;
      Under    : Flow.Computation.Model_Ref)
   return Boolean
   is

      Loop_Nodes : constant Flow.Node_List_T := Nodes (By, Under);
      -- The nodes in the list.

   begin

      return
         Flow.Computation.Is_Defined (
            Location => Location,
            By       => Nodes (By, Under),
            Under    => Under)
      or else (
         Opt.Include_Primitive
      and then
         Flow.Is_Defined (
            Location => Location,
            By       => Loop_Nodes));

   end Is_Defined;


end Loops.Cells;
