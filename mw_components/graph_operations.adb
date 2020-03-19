-- GENERAL OPERATIONS ON DIRECTED AND NONDIRECTED GRAPHS
   -----------------------------------------------------

-- Creation : 16-NOV-1987 by Mats Weber.


package body Graph_Operations is
-----------------------------

   procedure Contract (In_Graph          : in out Graph;
                       Grouped_Vertices  : in Vertex_Set;
                       Removed_Arcs      : in out Arc_Set;
                       Artificial_Vertex : out Vertex) is

      New_V : constant Vertex := New_Vertex;

      procedure Make_New_Arcs (V : in Vertex) is
      begin
         declare

            Leaving_Arcs : constant Arc_List := Leaving(V, In_Graph);

         begin
            for J in Leaving_Arcs'Range loop
               Set_Of_Arc.Add(Leaving_Arcs(J), To => Removed_Arcs);
               if not Set_Of_Vertex.Member(Final(Leaving_Arcs(J)), Grouped_Vertices) then
                  Add(New_Arc(Initial => New_V, Final => Final(Leaving_Arcs(J))), To => In_Graph);
               end if;
            end loop;
         end;
         declare

            Arriving_Arcs : constant Arc_List := Arriving(V, In_Graph);

         begin
            for J in Arriving_Arcs'Range loop
               if not Set_Of_Vertex.Member(Initial(Arriving_Arcs(J)), Grouped_Vertices) then
                  Set_Of_Arc.Add(Arriving_Arcs(J), To => Removed_Arcs);
                  Add(New_Arc(Initial => Initial(Arriving_Arcs(J)), Final => New_V), To => In_Graph);
               end if;
            end loop;
         end;
      end Make_New_Arcs;

      procedure Build_New_Arcs is new Set_Of_Vertex.Enumeration(Make_New_Arcs);

   begin
      Set_Of_Arc.Empty(Removed_Arcs);
      Add(New_V, To => In_Graph);
      Build_New_Arcs(Grouped_Vertices);
      Remove(Grouped_Vertices, From => In_Graph);
      Artificial_Vertex := New_V;
   end Contract;


   procedure Expand (In_Graph            : in out Graph;
                     Artificial_Vertex   : in Vertex;
                     Vertices_To_Restore : in Vertex_Set;
                     Arcs_To_Restore     : in Arc_Set) is
   begin
      Remove(Artificial_Vertex, From => In_Graph);
      Add(Vertices_To_Restore, To => In_Graph);
      Add(Arcs_To_Restore,     To => In_Graph);
   end Expand;

end Graph_Operations;
