-- GENERAL OPERATIONS ON NONDIRECTED GRAPHS
   ----------------------------------------

-- Creation : 16-NOV-1987 by Mats Weber.


with Graph_Handler,
     Dynamic_Arrays;

use Graph_Handler;

package Nondirected_Graph_Operations is
------------------------------------

   type Access_Arc_List is access Arc_List;

   package Array_Vertex_Of_Arc_List is new Dynamic_Arrays(Index_Type     => Vertex,
                                                          Component_Type => Access_Arc_List,
                                                          "<"            => Smaller,
                                                          Count          => Integer);


   procedure Find_Shortest_Paths (From           : in Vertex;
                                  In_Graph       : in Graph;
                                  Shortest_Paths : in out Array_Vertex_Of_Arc_List.Dynamic_Array);

   generic
      type Arc_Length is private;
      Zero : in Arc_Length;
      with function Length (Of_Arc : Arc) return Arc_Length is <>;
      with function "+" (A, B : Arc_Length) return Arc_Length is <>;
      with function "<" (A, B : Arc_Length) return Boolean is <>;
   procedure Find_Weighted_Shortest_Paths (From           : in Vertex;
                                           In_Graph       : in Graph;
                                           Shortest_Paths : in out Array_Vertex_Of_Arc_List.Dynamic_Array);


   function Shortest_Path (From, To : Vertex; In_Graph : Graph) return Arc_List;

   generic
      type Arc_Length is private;
      Zero : in Arc_Length;
      with function Length (Of_Arc : Arc) return Arc_Length is <>;
      with function "+" (A, B : Arc_Length) return Arc_Length is <>;
      with function "<" (A, B : Arc_Length) return Boolean is <>;
   function Weighted_Shortest_Path (From, To : Vertex; In_Graph : Graph) return Arc_List;


   procedure Destroy (Arc_Lists : in out Array_Vertex_Of_Arc_List.Dynamic_Array);
      -- Frees the memory allocated to ARC_LISTS.


   type Graph_Traversal_Order is (Depth_First, Breadth_First);

   generic
      with procedure Action (On_Vertex : in Vertex);
   procedure Graph_Traversal (From     : in Vertex;
                              In_Graph : in Graph;
                              Order    : in Graph_Traversal_Order);

end Nondirected_Graph_Operations;
