-- GENERAL OPERATIONS ON DIRECTED GRAPHS
   -------------------------------------

-- Creation :  8-JUN-1988 by Mats Weber.


with Stacks,
     Queues;

package body Directed_Graph_Operations is
--------------------------------------

   procedure Graph_Traversal (From      : in Vertex;
                              In_Graph  : in Graph;
                              Order     : in Graph_Traversal_Order;
                              Direction : in Graph_Traversal_Direction) is

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


         function Adjacent (V : Vertex; In_Graph : Graph) return Vertex_List is
         begin
            case Direction is
               when Forward =>
                  return Succ(V, In_Graph);
               when Backward =>
                  return Pred(V, In_Graph);
            end case;
         end Adjacent;

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

end Directed_Graph_Operations;
