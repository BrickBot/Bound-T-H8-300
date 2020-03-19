-- GENERAL OPERATIONS ON DIRECTED AND NONDIRECTED GRAPHS
   -----------------------------------------------------

-- Creation : 16-NOV-1987 by Mats Weber.


with Graph_Handler;

use Graph_Handler;

package Graph_Operations is
------------------------

   procedure Contract (In_Graph          : in out Graph;
                       Grouped_Vertices  : in Vertex_Set;
                       Removed_Arcs      : in out Arc_Set;
                       Artificial_Vertex : out Vertex);

   procedure Expand (In_Graph            : in out Graph;
                     Artificial_Vertex   : in Vertex;
                     Vertices_To_Restore : in Vertex_Set;
                     Arcs_To_Restore     : in Arc_Set);

end Graph_Operations;
