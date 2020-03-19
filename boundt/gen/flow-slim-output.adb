-- Flow.Slim.Output (body)
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
-- $Date: 2015/10/24 20:05:48 $
--
-- $Log: flow-slim-output.adb,v $
-- Revision 1.3  2015/10/24 20:05:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2001-01-13 11:27:06  holsti
-- Removed unnecessary exception handlers (which were null).
-- Removed to-be's, and other editorial changes.
--
-- Revision 1.1  2000/08/23 09:19:43  parviain
-- First version, added function Show_Nodes.
--

with Ada.Text_IO;


package body Flow.Slim.Output is


   use Ada.Text_IO;


   procedure Show_Nodes (Graph : in Flow.Slim.Graph_T)
   --
   -- This procedure prints information of a slim Graph's
   -- nodes and edges.
   --
   is

      procedure Show_Edge (Edge : in Edge_Index_T)
      --
      -- Displays one edge, if it is bare or fluxed,
      -- its start and end nodes and its condition
      -- if the edge is bare.
      --
      is
         E : Edge_T := Edge_At (Edge, Graph);
      begin
         Put ("      Edge " & Edge_Index_T'Image(Edge) & " is ");
         if E.Kind = Bare
         then
            Put (" a bare ");
         else
            Put (" a fluxed ");
         end if;

         Put ("edge : ");

         Put (Node_Index_T'Image(Index(Source(E))) & " -> " &
              Node_Index_T'Image(Index(Target(E))));

         case E.Kind is

            when Bare =>

               Put (" when " & Arithmetic.Image(Condition(E)));

            when Fluxed =>
               -- Put (Calculator.Flux_T'Image(Flux(E)));

               null;

         end case;

         New_Line;

      end Show_Edge;


      procedure Show_Node (Node : in Node_Index_T)
      --
      -- Shows one node, its type (basic/fusion)
      -- and its leaving and entering edges.
      --
      is
         N     : Node_T      := Node_At (Node, Graph);
         E_In  : Edge_List_T := Edges_Into (N, Graph);
         E_Out : Edge_List_T := Edges_From (N, Graph);

      begin

         Put ("Node " & Node_Index_T'Image(Node) & " is a ");

         case N.Kind is
            when Basic =>
               Put ("basic ");
            when Fusion =>
               Put ("fusion ");
         end case;

         Put_Line ("node.");

         Put_Line ("   Edges in: " & Positive'Image(E_In'Last));

         for E in E_In'Range loop
            Show_Edge (Index(E_In(E)));
         end loop;

         Put_Line ("   Edges out: " & Positive'Image(E_In'Last));

         for E in E_In'Range loop
            Show_Edge (Index(E_Out(E)));
         end loop;

      end Show_Node;


   begin  -- Show_Nodes;

      Put_Line ("Slimmed flow graph nodes and node edges :");

      for I in 1..Max_Node (Graph) loop
         Show_Node (I);
      end loop;

      Put_Line ("End of flow graph.");

   end Show_Nodes;


end Flow.Slim.Output;
