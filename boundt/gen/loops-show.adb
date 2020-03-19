-- Loops.Show (body)
--
-- Textual output of control-flow loop-structures.
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
-- $Log: loops-show.adb,v $
-- Revision 1.4  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.3  2009-01-18 07:57:00  niklas
-- Removed unused context clause.
--
-- Revision 1.2  2004/04/25 13:47:54  niklas
-- First Tidorum version. Include surrounding source lines.
--
-- Revision 1.1  2001/03/21 20:31:24  holsti
-- First version.
--


with Text_IO;

with Flow;
with Flow.Show;


package body Loops.Show is

   use  Text_IO;


   function Locus (
      Luup   : Loop_T;
      Within : Flow.Graph_T;
      Source : Symbols.Symbol_Table_T)
   return Output.Locus_T
   is

      Loc : Output.Locus_T;
      -- The result.

   begin

      Loc :=
         Output.Locus (
            Statements =>
               Flow.Show.Statements (
                  Steps =>
                     Steps_In (
                        Luup   => Luup, 
                        Within => Within),
                  Source => Source));

      Output.Add_Surrounding_Lines (To => Loc);

      return Loc;

   end Locus;


   Column_Width : constant Positive_Count := 3;
   --
   -- This parameter controls the format for the output.


   type Loop_Done_T is array (Loop_Index_T range <>) of Boolean;
   --
   -- This array type is just to mark loops we have already done.


   procedure Show_Loops (Luups : in Loops_T)
   --
   -- Displays the given loop-structure, including head and
   -- member nodes, and the loop-nesting hierarchy.
   -- Results on standard output.
   is

      use type Flow.Node_Set_T;

      Luups_Done : Loop_Done_T (Luups'Range) := (others => False);
      --
      -- Whether a given loop has been shown already.


      procedure Print_Loop (
         L      : in Loop_Index_T;
         Column : in Positive_Count)
      is
      --
      -- This procedure prints the loop index L and on the next
      -- line all node numbers in the loop.
      -- After this it prints all the loops contained in
      -- L, indented by Column_Width.

         Node_Set : constant Flow.Node_Set_Ref := Members (Luups(L));
         -- The nodes in loop number L.

         Nodes : constant Flow.Node_Index_List_T :=
            Flow.To_Index_List (Node_Set.all);
         -- The (indices of the) nodes in loop number L.

      begin
         Luups_Done (L) := True;

         Set_Col (Column);
         Put_Line ("Loop #" & Loop_Index_T'Image(L));
         Set_Col (Column);
         Put ("Head node : " );
         Put_Line ( Flow.Node_Index_T'Image(
            Flow.Index(Head_Node(Luups(L)))));

         Set_Col (Column);
         Put ("Nodes : ");

         for I in Nodes'Range loop
            Put (" " & Flow.Node_Index_T'Image(Nodes(I)));
         end loop;

         New_Line;

         for SL in reverse Luups'First..(L - 1) loop
            declare
               Sub_Loop_Nodeset : Flow.Node_Set_Ref :=
                  Members (Luups(SL));
            begin
               if Sub_Loop_Nodeset.all <= Node_Set.all then
                  Print_Loop (SL, Column + Column_Width);
               end if;
            end;
         end loop;

      end;

   begin -- Show_Loops
      Put_Line ("Loops and their nodes : ");
      for L in reverse Luups'Range loop
         if not Luups_Done (L) then
            Print_Loop(L, 1);
         end if;
      end loop;

   end Show_Loops;


   procedure Show_Loop (Luup : in Loop_T) is
   begin
      Show_Loops ((1 => Luup));
   end;


end Loops.Show;
