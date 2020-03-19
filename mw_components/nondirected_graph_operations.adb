-- GENERAL OPERATIONS ON NONDIRECTED GRAPHS
   ----------------------------------------

-- Creation :  7-DEC-1987 by Mats Weber.


with Stacks,
     Queues,
     Unchecked_Deallocation;

package body Nondirected_Graph_Operations is

   Not_Yet_Implemented : exception;
   --\--\--\--\--\--\--\--\--\--\--


   function Unit_Length (A : Arc) return Positive is
   begin
      return 1;
   end;


   procedure Find_Shortest_Paths (From           : in Vertex;
                                  In_Graph       : in Graph;
                                  Shortest_Paths : in out Array_Vertex_Of_Arc_List.Dynamic_Array) is

      procedure Find_Shortest_Paths is new Find_Weighted_Shortest_Paths (Arc_Length => Natural,
                                                                         Zero       => 0,
                                                                         Length     => Unit_Length);

   begin
      Find_Shortest_Paths (From, In_Graph, Shortest_Paths);
   end Find_Shortest_Paths;


   procedure Find_Weighted_Shortest_Paths (From           : in Vertex;
                                           In_Graph       : in Graph;
                                           Shortest_Paths : in out Array_Vertex_Of_Arc_List.Dynamic_Array) is
   begin
      Destroy(Shortest_Paths);
      raise Not_Yet_Implemented;
   end Find_Weighted_Shortest_Paths;


   function Shortest_Path (From, To : Vertex; In_Graph : Graph) return Arc_List is

      function Shortest_Path is new Weighted_Shortest_Path (Arc_Length => Natural,
                                                            Zero       => 0,
                                                            Length     => Unit_Length);

   begin
      return Shortest_Path(From, To, In_Graph);
   end Shortest_Path;


   function Weighted_Shortest_Path (From, To : Vertex; In_Graph : Graph) return Arc_List is

      procedure Find_Shortest_Paths is new Find_Weighted_Shortest_Paths(Arc_Length, Zero);

      All_Shortest_Paths : Array_Vertex_Of_Arc_List.Dynamic_Array;

   begin
      Find_Shortest_Paths(From, In_Graph, All_Shortest_Paths);
      declare

         Result : constant Arc_List := Array_Vertex_Of_Arc_List.Component(Index => To,
                                                                          From  => All_Shortest_Paths).all;

      begin
         Destroy(All_Shortest_Paths);
         return Result;
      end;
   end Weighted_Shortest_Path;


   procedure Destroy (Arc_Lists : in out Array_Vertex_Of_Arc_List.Dynamic_Array) is

      procedure Destroy (V : in Vertex; Al : in out Access_Arc_List) is

         procedure Dispose is new Unchecked_Deallocation(Arc_List,
                                                         Access_Arc_List);

      begin
         Dispose(Al);
      end Destroy;

      procedure Destroy_All is new Array_Vertex_Of_Arc_List.Update_All(Destroy);

   begin
      Destroy_All(Arc_Lists);
      Array_Vertex_Of_Arc_List.Destroy(Arc_Lists);
   end Destroy;


   procedure Graph_Traversal (From     : in Vertex;
                              In_Graph : in Graph;
                              Order    : in Graph_Traversal_Order) is

      generic
         type Vertex_Storage is limited private;
         with procedure Store (Item : in Vertex; Into : in out Vertex_Storage);
         with procedure Retrieve (From : in out Vertex_Storage; Item : out Vertex);
         with function Empty (Storage : Vertex_Storage) return Boolean is <>;
         with procedure Destroy (Storage : in out Vertex_Storage) is <>;
      procedure Graph_Traversal;

      procedure Graph_Traversal is

         Waiting_Vertices : Vertex_Storage;
         Marked_Vertices  : Vertex_Set;
         V                : Vertex;

         use Set_Of_Vertex;

      begin
         Add(From, To => Marked_Vertices);
         Store(From, Into => Waiting_Vertices);
         while not Empty(Waiting_Vertices) loop
            Retrieve(Item => V, From => Waiting_Vertices);
            begin
               Action(V);
            exception
               when others =>
                  Destroy(Waiting_Vertices);
                  Empty(Marked_Vertices);
                  raise;
            end;
            declare

               Adjacent_To_V : constant Vertex_List := Adjacent(V, In_Graph);

            begin
               for I in Adjacent_To_V'Range loop
                  if not Member(Adjacent_To_V(I), Marked_Vertices) then
                     Add(Adjacent_To_V(I), To => Marked_Vertices);
                     Store(Adjacent_To_V(I), Into => Waiting_Vertices);
                  end if;
               end loop;
            end;
         end loop;
         Empty(Marked_Vertices);
      end Graph_Traversal;

   begin
      case Order is
         when Depth_First =>
            declare

               package Vertex_Stacks is new Stacks(Vertex, Count => Natural);

               procedure Traverse_Depth_First is new Graph_Traversal(Vertex_Storage => Vertex_Stacks.Stack,
                                                                     Store          => Vertex_Stacks.Push,
                                                                     Retrieve       => Vertex_Stacks.Pop,
                                                                     Empty          => Vertex_Stacks.Empty,
                                                                     Destroy        => Vertex_Stacks.Destroy);

            begin
               Traverse_Depth_First;
            end;
         when Breadth_First =>
            declare

               package Vertex_Queues is new Queues(Vertex, Count => Natural);

               procedure Traverse_Breadth_First is new Graph_Traversal(Vertex_Storage => Vertex_Queues.Queue,
                                                                       Store          => Vertex_Queues.Put,
                                                                       Retrieve       => Vertex_Queues.Get,
                                                                       Empty          => Vertex_Queues.Empty,
                                                                       Destroy        => Vertex_Queues.Destroy);

            begin
               Traverse_Breadth_First;
            end;
      end case;
   end Graph_Traversal;

end Nondirected_Graph_Operations;
