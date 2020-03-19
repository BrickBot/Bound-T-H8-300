-- PACKAGE FOR GRAPH HANDLING
   --------------------------

-- Revision : 20-SEP-1986 by Mats Weber, use renaming declarations.
-- Revision :  9-SEP-1986 by Mats Weber, added degree functions.
-- Revision :  3-JUN-1986 by Mats Weber

-- Creation : 19-FEB-1986 by Mats Weber


with Quick_Sort_Function,
     Unchecked_Deallocation;

package body Graph_Handler is
--------------------------

  procedure Add (The_Vertex : in Vertex; To : in out Graph) is
  begin
    Vertex_Arc.Check(The_Vertex);
    Insert(Key => The_Vertex, Item => new Adj_Arcs_Rec, Into => To);
  exception
    when Graph_Table.Duplicate_Key => null;
  end Add;


  procedure Add (Vertices : in Vertex_Set; To : in out Graph) is

    procedure Add_To_To (The_Vertex : in Vertex) is
    begin
      Add(The_Vertex, To => To);
    end;

    procedure Add_Vertices_To_To is new Set_Of_Vertex.Enumeration(Add_To_To);

  begin
    Add_Vertices_To_To(Vertices);
  end Add;


  procedure Add (Vertices : in Vertex_List; To : in out Graph) is
  begin
    for I in Vertices'Range loop
      Add(Vertices(I), To => To);
    end loop;
  end Add;


  procedure Add (The_Arc : in Arc; To : in out Graph) is
  begin
    Vertex_Arc.Check(The_Arc);
    declare

      Adj_Arcs_Initial : Adj_Arcs_Rec renames Search(Initial(The_Arc), To).all;
      Adj_Arcs_Final   : Adj_Arcs_Rec renames Search(Final(The_Arc), To).all;

    begin
      Set_Of_Arc.Add(The_Arc, Adj_Arcs_Initial.Leaving);
      Set_Of_Arc.Add(The_Arc, Adj_Arcs_Final.Arriving);
    end;
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end Add;


  procedure Add (Arcs : in Arc_Set; To : in out Graph) is

    procedure Add_To_To (The_Arc : in Arc) is
    begin
      Add(The_Arc, To => To);
    end;

    procedure Add_Arcs_To_To is new Set_Of_Arc.Enumeration(Add_To_To);

  begin
    Add_Arcs_To_To(Arcs);
  end Add;


  procedure Add (Arcs : in Arc_List; To : in out Graph) is
  begin
    for I in Arcs'Range loop
      Add(Arcs(I), To => To);
    end loop;
  end Add;


  procedure Dispose is new Unchecked_Deallocation(Adj_Arcs_Rec, Adj_Arcs);


  procedure Remove (The_Vertex : in Vertex; From : in out Graph) is

    The_Vertex_Adj_Arcs : Adj_Arcs;

    procedure Remove_From_Predecessor (The_Arc : in Arc) is
    begin
      Set_Of_Arc.Remove(The_Arc, From => Search(Initial(The_Arc), From).Leaving);
    end;

    procedure Update_Predecessors is new Set_Of_Arc.Enumeration(Remove_From_Predecessor);


    procedure Remove_From_Successor (The_Arc : in Arc) is
    begin
      Set_Of_Arc.Remove(The_Arc, From => Search(Final(The_Arc), From).Arriving);
    end;

    procedure Update_Successors is new Set_Of_Arc.Enumeration(Remove_From_Successor);

  begin
    Vertex_Arc.Check(The_Vertex);
    Remove(Key => The_Vertex, From => From, Removed_Item => The_Vertex_Adj_Arcs);
    Update_Predecessors(The_Vertex_Adj_Arcs.Arriving);
    Set_Of_Arc.Empty(The_Vertex_Adj_Arcs.Arriving);
    Update_Successors(The_Vertex_Adj_Arcs.Leaving);
    Set_Of_Arc.Empty(The_Vertex_Adj_Arcs.Leaving);
    Dispose(The_Vertex_Adj_Arcs);
  exception
    when Graph_Table.Nonexistent_Key => null;
  end Remove;


  procedure Remove (Vertices : in Vertex_Set; From : in out Graph) is

    procedure Remove_From_From (The_Vertex : in Vertex) is
    begin
      Remove(The_Vertex, From => From);
    end;

    procedure Remove_Vertices_From_From is new Set_Of_Vertex.Enumeration(Remove_From_From);

  begin
    Remove_Vertices_From_From(Vertices);
  end Remove;


  procedure Remove (Vertices : in Vertex_List; From : in out Graph) is
  begin
    for I in Vertices'Range loop
      Remove(Vertices(I), From => From);
    end loop;
  end Remove;


  procedure Remove (The_Arc : in Arc; From : in out Graph) is
  begin
    Vertex_Arc.Check(The_Arc);
    Set_Of_Arc.Remove(The_Arc, From => Search(Initial(The_Arc), From).Leaving);
    Set_Of_Arc.Remove(The_Arc, From => Search(Final(The_Arc), From).Arriving);
  exception
    when Graph_Table.Nonexistent_Key => null;
  end Remove;


  procedure Remove (Arcs : Arc_Set; From : in out Graph) is

    procedure Remove_From_From (The_Arc : in Arc) is
    begin
      Remove(The_Arc, From => From);
    end;

    procedure Remove_Arcs_From_From is new Set_Of_Arc.Enumeration(Remove_From_From);

  begin
    Remove_Arcs_From_From(Arcs);
  end Remove;


  procedure Remove (Arcs : Arc_List; From : in out Graph) is
  begin
    for I in Arcs'Range loop
      Remove(Arcs(I), From => From);
    end loop;
  end Remove;


  function Member (The_Vertex : Vertex; Of_Graph : Graph) return Boolean is
  begin
    Vertex_Arc.Check(The_Vertex);
    return Graph_Table.Member(The_Vertex, Graph_Table.Table(Of_Graph));
  end Member;


  function Member (The_Arc : Arc; Of_Graph : Graph) return Boolean is
  begin
    Vertex_Arc.Check(The_Arc);
    return Set_Of_Arc.Member(The_Arc, Search(Initial(The_Arc), Of_Graph).Leaving);
  exception
    when Graph_Table.Nonexistent_Key =>
      return False;
  end Member;


  function Some_Vertex (In_Graph : Graph) return Vertex is
  begin
    return Min(In_Graph);
  exception
    when Graph_Table.Table_Empty =>
      raise No_Vertices_In_Graph;
  end Some_Vertex;


  function Some_Arc (In_Graph : Graph) return Arc is

    An_Arc    : Arc;

    Found_Arc : exception;

    procedure Remember_An_Arc (The_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is
    begin
      if not Set_Of_Arc.Empty(Its_Arcs.Arriving) then
        An_Arc := Set_Of_Arc.Min(Its_Arcs.Arriving);
        raise Found_Arc;
      elsif not Set_Of_Arc.Empty(Its_Arcs.Leaving) then
        An_Arc := Set_Of_Arc.Min(Its_Arcs.Leaving);
        raise Found_Arc;
      end if;
    end Remember_An_Arc;

    procedure Search_An_Arc is new Graph_Table.Traversal(Action => Remember_An_Arc);

  begin
    Search_An_Arc(Graph_Table.Table(In_Graph));
    raise No_Arcs_In_Graph;
  exception
    when Found_Arc =>
      return An_Arc;
  end Some_Arc;


  function Pred (Of_Vertex : Vertex; In_Graph : Graph) return Vertex_List is
  begin
    Vertex_Arc.Check(Of_Vertex);
    declare

      Arriving_Arcs : Arc_Set renames Search(Of_Vertex, In_Graph).Arriving;
      Result        : Vertex_List(1..Set_Of_Arc.Card(Arriving_Arcs));
      N             : Integer range 0..Result'Last := 0;

      procedure Put_Into_Result(The_Arc : in Arc) is
      begin
        N := N + 1;
        Result(N) := Initial(The_Arc);
      end;

      procedure Traverse_And_Put_Into_Result is new Set_Of_Arc.Enumeration(Put_Into_Result);

    begin
      Traverse_And_Put_Into_Result(Arriving_Arcs);
      return Result;
    end;
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end Pred;


  function Succ (Of_Vertex : Vertex; In_Graph : Graph) return Vertex_List is
  begin
    Vertex_Arc.Check(Of_Vertex);
    declare

      Leaving_Arcs : Arc_Set renames Search(Of_Vertex, In_Graph).Leaving;
      Result       : Vertex_List(1..Set_Of_Arc.Card(Leaving_Arcs));
      N            : Integer range 0..Result'Last := 0;

      procedure Put_Into_Result(The_Arc : in Arc) is
      begin
        N := N + 1;
        Result(N) := Final(The_Arc);
      end;

      procedure Traverse_And_Put_Into_Result is new Set_Of_Arc.Enumeration(Put_Into_Result);

    begin
      Traverse_And_Put_Into_Result(Leaving_Arcs);
      return Result;
    end;
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end Succ;


  function Adjacent (To_Vertex : Vertex; In_Graph : Graph) return Vertex_List is

    function Sorted_List is new Quick_Sort_Function(Index  => Positive,
                                                    Item   => Vertex,
                                                    "<"    => Smaller,
                                                    Vector => Vertex_List);

    function "&" (Left, Right : Vertex_List) return Vertex_List renames Set_Of_Vertex."&";


    function To_Vertex_Deleted (Vertices : Vertex_List) return Vertex_List is
      -- Returns VERTICES with all copies of TO_VERTEX removed.

      Result : Vertex_List(Vertices'Range) := Vertices;
      Last   : Integer range Result'First - 1..Result'Last := Result'Last;
      I      : Integer range Result'First..Result'Last + 1 := Result'First;

    begin
      Main :
        while I <= Last loop
          while Result(I) = To_Vertex loop
            Result(I..Last - 1) := Result(I + 1..Last);
            Last := Last - 1;
            exit Main when I > Last;
          end loop;
          I := I + 1;
        end loop Main;
      return Result(Result'First..Last);
    end To_Vertex_Deleted;

  begin
    Vertex_Arc.Check(To_Vertex);
    return Sorted_List(Pred(To_Vertex, In_Graph) & To_Vertex_Deleted(Succ(To_Vertex, In_Graph)));
      -- TO_VERTEX must be returned only once for each loop from TO_VERTEX to TO_VERTEX.
  end Adjacent;


  function Arriving (To_Vertex : Vertex; In_Graph : Graph) return Arc_List is
  begin
    Vertex_Arc.Check(To_Vertex);
    return Set_Of_Arc.To_List(Search(To_Vertex, In_Graph).Arriving);
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end;


  function Leaving (The_Vertex : Vertex; In_Graph : Graph) return Arc_List is
  begin
    Vertex_Arc.Check(The_Vertex);
    return Set_Of_Arc.To_List(Search(The_Vertex, In_Graph).Leaving);
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end;


  function Adjacent (To_Vertex : Vertex; In_Graph : Graph) return Arc_List is

    function Sorted_List is new Quick_Sort_Function(Index  => Positive,
                                                    Item   => Arc,
                                                    "<"    => Smaller,
                                                    Vector => Arc_List);

    function To_Vertex_Loopless (Arcs : Arc_List) return Arc_List is
      -- Returns ARCS with all loops from TO_VETEX to TO_VERTEX removed.

      Result : Arc_List(Arcs'Range) := Arcs;
      Last   : Integer range Result'First - 1..Result'Last := Result'Last;
      I      : Integer range Result'First..Result'Last + 1 := Result'First;

    begin
      Main :
        while I <= Last loop
          while Initial(Result(I)) = To_Vertex and Final(Result(I)) = To_Vertex loop
            Result(I..Last - 1) := Result(I + 1..Last);
            Last := Last - 1;
            exit Main when I > Last;
          end loop;
          I := I + 1;
        end loop Main;
      return Result(Result'First..Last);
    end To_Vertex_Loopless;

  begin
    Vertex_Arc.Check(To_Vertex);
    declare

      To_Vertex_Adj_Arcs : Adj_Arcs_Rec renames Search(To_Vertex, In_Graph).all;

      function "&" (Left, Right : Arc_List) return Arc_List renames Set_Of_Arc."&";

    begin
      return Sorted_List(Set_Of_Arc.To_List(To_Vertex_Adj_Arcs.Arriving) &
                         To_Vertex_Loopless(Set_Of_Arc.To_List(To_Vertex_Adj_Arcs.Leaving)));
        -- loops from TO_VERTEX to TO_VERTEX must be returned only once.
    end;
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end Adjacent;


  function In_Degree (Of_Vertex : Vertex; In_Graph : Graph) return Natural is
  begin
    Vertex_Arc.Check(Of_Vertex);
    return Set_Of_Arc.Card(Search(Of_Vertex, In_Graph).Arriving);
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end In_Degree;


  function Out_Degree (Of_Vertex : Vertex; In_Graph : Graph) return Natural is
  begin
    Vertex_Arc.Check(Of_Vertex);
    return Set_Of_Arc.Card(Search(Of_Vertex, In_Graph).Leaving);
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end Out_Degree;


  function Degree (Of_Vertex : Vertex; In_Graph : Graph) return Natural is
  begin
    Vertex_Arc.Check(Of_Vertex);
    declare

      Of_Vertex_Adj_Arcs : Adj_Arcs_Rec renames Search(Of_Vertex, In_Graph).all;

    begin
      return Set_Of_Arc.Card(Of_Vertex_Adj_Arcs.Arriving) +
             Set_Of_Arc.Card(Of_Vertex_Adj_Arcs.Leaving);
    end;
  exception
    when Graph_Table.Nonexistent_Key =>
      raise Vertex_Not_In_Graph;
  end Degree;


  function Vertices (In_Graph : Graph) return Vertex_List is

    Result : Vertex_List(1..Card(In_Graph));
    N      : Integer range 0..Result'Last := 0;

    procedure Put_Into_Result (The_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is
    begin
      N := N + 1;
      Result(N) := The_Vertex;
    end;

    procedure Traverse_And_Put_Into_Result is new Graph_Table.Traversal(Put_Into_Result);

  begin
    Traverse_And_Put_Into_Result(Graph_Table.Table(In_Graph));
    return Result;
  end Vertices;


  function Arcs (In_Graph : Graph) return Arc_List is

    N_Arcs     : constant Natural := Number_Of_Arcs(In_Graph);

    All_Arcs   : Arc_List(1..N_Arcs);
    Curr_Index : Integer range 0..N_Arcs := 0;


    procedure Add_To_All_Arcs (The_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is

      procedure Add_One_Arc (The_Arc : in Arc) is
      begin
        Curr_Index := Curr_Index + 1;
        All_Arcs(Curr_Index) := The_Arc;
      end;

      procedure Traverse_Arcs is new Set_Of_Arc.Enumeration(Add_One_Arc);

    begin
      Traverse_Arcs(Its_Arcs.Leaving);
    end Add_To_All_Arcs;

    procedure Traverse_And_Add_To_All_Arcs is new Graph_Table.Traversal(Add_To_All_Arcs);

  begin
    Traverse_And_Add_To_All_Arcs(Graph_Table.Table(In_Graph));
    return All_Arcs;
  end Arcs;


  function Number_Of_Vertices (In_Graph : Graph) return Natural is
  begin
    return Card(In_Graph);
  end Number_Of_Vertices;


  function Number_Of_Arcs (In_Graph : Graph) return Natural is

    Result : Natural := 0;

    procedure Count_Arcs (The_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is
    begin
      Result := Result + Set_Of_Arc.Card(Its_Arcs.Leaving);
    end;

    procedure Traverse_And_Count_Arcs is new Graph_Table.Traversal(Count_Arcs);

  begin
    Traverse_And_Count_Arcs(Graph_Table.Table(In_Graph));
    return Result;
  end Number_Of_Arcs;


  procedure Vertex_Enumeration (On_Graph : in Graph) is

    procedure New_Action (On_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is
    begin
      Action(On_Vertex);
    end;

    procedure Enumerate is new Graph_Table.Traversal(New_Action);

  begin
    Enumerate(Graph_Table.Table(On_Graph));
  end Vertex_Enumeration;


  procedure Arc_Enumeration (On_Graph : in Graph) is

    procedure Vertex_Action (On_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is

      procedure Enumerate_Arcs is new Set_Of_Arc.Enumeration(Action);

    begin
      Enumerate_Arcs(Its_Arcs.Leaving);
    end;

    procedure Enumerate_Vertices is new Graph_Table.Traversal(Vertex_Action);

  begin
    Enumerate_Vertices(Graph_Table.Table(On_Graph));
  end Arc_Enumeration;


  procedure Assign (Object : in out Graph; Value : in Graph) is

    procedure Add_To_Object (The_Vertex : in Vertex; Its_Arcs : in Adj_Arcs) is

      function Duplicate_Adj_Arcs (The_Adj_Arcs : Adj_Arcs) return Adj_Arcs is

        New_Adj_Arcs : constant Adj_Arcs := new Adj_Arcs_Rec;

      begin
        Set_Of_Arc.Add(The_Adj_Arcs.Arriving, New_Adj_Arcs.Arriving);
        Set_Of_Arc.Add(The_Adj_Arcs.Leaving,  New_Adj_Arcs.Leaving);
        return New_Adj_Arcs;
      end;

    begin
      Insert(Key => The_Vertex, Item => Duplicate_Adj_Arcs(Its_Arcs), Into => Object);
    end Add_To_Object;

    procedure Traverse_And_Add_To_Object is new Graph_Table.Traversal(Add_To_Object);

  begin
    Destroy(Object);
    Traverse_And_Add_To_Object(Graph_Table.Table(Value));
  end Assign;


  procedure Destroy (The_Graph : in out Graph) is

    procedure Destroy_Adj_Arcs (The_Vertex : in Vertex; Its_Arcs : in out Adj_Arcs) is
    begin
      Set_Of_Arc.Empty(Its_Arcs.Arriving);
      Set_Of_Arc.Empty(Its_Arcs.Leaving);
      Dispose(Its_Arcs);
    end;

    procedure Remove_All_Adj_Arcs is new Graph_Table.Update_All(Destroy_Adj_Arcs);

  begin
    Remove_All_Adj_Arcs(Graph_Table.Table(The_Graph));
    Graph_Table.Destroy(Graph_Table.Table(The_Graph));
  end Destroy;


  procedure Swap (Left, Right : in out Graph) is
  begin
    Graph_Table.Swap(Graph_Table.Table(Left), Graph_Table.Table(Right));
  end Swap;

end Graph_Handler;
