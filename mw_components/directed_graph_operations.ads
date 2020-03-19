-- GENERAL OPERATIONS ON DIRECTED GRAPHS
   -------------------------------------

-- Creation : 18-NOV-1987 by Mats Weber.


with Graph_Handler;

use Graph_Handler;

package Directed_Graph_Operations is
---------------------------------

   type Graph_Traversal_Order is (Depth_First, Breadth_First);

   type Graph_Traversal_Direction is (Forward, Backward);

   generic
      with procedure Action (On_Vertex : in Vertex);
   procedure Graph_Traversal (From      : in Vertex;
                              In_Graph  : in Graph;
                              Order     : in Graph_Traversal_Order;
                              Direction : in Graph_Traversal_Direction);

end Directed_Graph_Operations;
