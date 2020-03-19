-- PACKAGE FOR GRAPH HANDLING
   --------------------------

-- Revision : 31-AUG-1988 by Mats Weber, added procedure SWAP(GRAPH, GRAPH).
-- Revision : 27-JUN-1988 by Mats Weber, renamed package from SARTEX to GRAPH_HANDLER.
-- Revision : 20-APR-1988 by Mats Weber, added functions SOME_VERTEX and SOME_ARC.
-- Revision :  7-DEC-1987 by Mats Weber, added function OTHER_END(ARC, VERTEX) return VERTEX.
-- Revision : 16-NOV-1987 by Mats Weber, added procedures ADD(SET, GRAPH) and
--                                       ADD(LIST, GRAPH) and changed parameter
--                                       names.
-- Revision : 11-NOV-1987 by Mats Weber, stop using access types
--                                       for VERTEX and ARC.
-- Revision :  9-SEP-1986 by Mats Weber, added degree functions.
-- Revision :  3-JUN-1986 by Mats Weber.

-- Creation :  4-JUL-1985 by Mats Weber.


with Vertex_Arc,
     General_Sets,
     Tables;

package Graph_Handler is
---------------------

  -- type VERTEX and related procedures --
  ----------------------------------------

  subtype Vertex is Vertex_Arc.Vertex;
  --------------

  function "=" (V1, V2 : Vertex) return Boolean renames Vertex_Arc."=";

  function Smaller (V1, V2 : Vertex) return Boolean renames Vertex_Arc.Smaller;
    -- Ordering function on vertices.

  function New_Vertex return Vertex renames Vertex_Arc.New_Vertex;
    -- Creates a new vertex.

  package Set_Of_Vertex is new General_Sets(Element_Type => Vertex, "<" => Smaller, Count => Integer);
  ---------------------

  subtype Vertex_List is Set_Of_Vertex.List;  -- array (POSITIVE range <>) of VERTEX;
  subtype Vertex_Set  is Set_Of_Vertex.Set;


  -- type ARC and related procedures --
  -------------------------------------

  subtype Arc is Vertex_Arc.Arc;
  -----------

  function New_Arc (Initial, Final : Vertex) return Arc renames Vertex_Arc.New_Arc;
    -- Creates a new arc.

  function Initial (Of_Arc : Arc) return Vertex renames Vertex_Arc.Initial;
    -- Returns the initial vertex of OF_ARC.

  function Final (Of_Arc : Arc) return Vertex renames Vertex_Arc.Final;
    -- Returns the final vertex of OF_ARC.

  function Other_End (Of_Arc : Arc; This_End : Vertex) return Vertex renames Vertex_Arc.Other_End;
    -- Returns FINAL(OF_ARC) if THIS_END = INITIAL(OF_ARC),
    --         INITIAL(OF_ARC) if THIS_END = FINAL(OF_ARC),
    -- raises VERTEX_NOT_ON_ARC otherwise.

  function "=" (A1, A2 : Arc) return Boolean renames Vertex_Arc."=";

  function Smaller (A1, A2 : Arc) return Boolean renames Vertex_Arc.Smaller;
    -- Ordering function on arcs.

  package Set_Of_Arc is new General_Sets(Element_Type => Arc, "<" => Smaller, Count => Integer);
  ------------------

  subtype Arc_List is Set_Of_Arc.List;  -- array (POSITIVE range <>) of ARC;
  subtype Arc_Set  is Set_Of_Arc.Set;

  Vertex_Not_On_Arc : exception renames Vertex_Arc.Vertex_Not_On_Arc;


  -- type GRAPH and related procedures --
  ---------------------------------------

  type Graph is limited private;
  ----------

  procedure Add (The_Vertex : in Vertex; To : in out Graph);
    -- Adds THE_VERTEX to TO. No exception is raised if THE_VERTEX is already in TO.

  procedure Add (Vertices : in Vertex_Set;  To : in out Graph);
  procedure Add (Vertices : in Vertex_List; To : in out Graph);


  procedure Add (The_Arc : in Arc; To : in out Graph);
    -- Adds THE_ARC to TO. No exception is raised if THE_ARC is already in TO.
    -- Raises VERTEX_NOT_IN_GRAPH if INITIAL(THE_ARC) or FINAL(THE_ARC) are
    -- not in TO.

  procedure Add (Arcs : in Arc_Set;  To : in out Graph);
  procedure Add (Arcs : in Arc_List; To : in out Graph);


  procedure Remove (The_Vertex : in Vertex; From : in out Graph);
    -- Removes THE_VERTEX from FROM along with THE_VERTEX's adjacent arcs.
    -- No exception is raised if THE_VERTEX is not found.

  procedure Remove (Vertices : in Vertex_Set;  From : in out Graph);
  procedure Remove (Vertices : in Vertex_List; From : in out Graph);


  procedure Remove (The_Arc : in Arc; From : in out Graph);
    -- Removes THE_ARC from FROM. No exception is raised if THE_ARC is not found.

  procedure Remove (Arcs : Arc_Set;  From : in out Graph);
  procedure Remove (Arcs : Arc_List; From : in out Graph);


  function Member (The_Vertex : Vertex; Of_Graph : Graph) return Boolean;
  function Member (The_Arc    : Arc;    Of_Graph : Graph) return Boolean;


  function Some_Vertex (In_Graph : Graph) return Vertex;
    -- Returns some vertex in IN_GRAPH.
    -- Raises NO_VERTICES_IN_GRAPH if IN_GRAPH contains no vertices.

  function Some_Arc (In_Graph : Graph) return Arc;
    -- Returns some arc in IN_GRAPH.
    -- Raises NO_ARCS_IN_GRAPH if IN_GRAPH contains no arcs.


  function Pred     (Of_Vertex : Vertex; In_Graph : Graph) return Vertex_List;
  function Succ     (Of_Vertex : Vertex; In_Graph : Graph) return Vertex_List;
  function Adjacent (To_Vertex : Vertex; In_Graph : Graph) return Vertex_List;
    -- Return an ordered list of the predecessors, successors or
    -- adjacent vertices of the vertex in IN_GRAPH.
    -- If two arcs lead from the vertex to another vertex, then this vertex
    -- is returned twice.
    -- Raise VERTEX_NOT_IN_GRAPH if the vertex is not in IN_GRAPH.

  function Arriving (To_Vertex  : Vertex; In_Graph : Graph) return Arc_List;
  function Leaving  (The_Vertex : Vertex; In_Graph : Graph) return Arc_List;
  function Adjacent (To_Vertex  : Vertex; In_Graph : Graph) return Arc_List;
    -- Return an ordered list of the arcs arriving, leaving or adjacent to the vertex.
    -- Raise VERTEX_NOT_IN_GRAPH if the vertex is not in IN_GRAPH.

  function In_Degree  (Of_Vertex : Vertex; In_Graph : Graph) return Natural;
  function Out_Degree (Of_Vertex : Vertex; In_Graph : Graph) return Natural;
  function Degree     (Of_Vertex : Vertex; In_Graph : Graph) return Natural;
    -- Return the in-degree, out-degree or degree of OF_VERTEX.


  function Vertices (In_Graph : Graph) return Vertex_List;
    -- Returns an ordered list of all vertices in IN_GRAPH.

  function Arcs (In_Graph : Graph) return Arc_List;
    -- Returns an ordered list of all arcs in IN_GRAPH.

  function Number_Of_Vertices (In_Graph : Graph) return Natural;
  function Number_Of_Arcs     (In_Graph : Graph) return Natural;


  generic
    with procedure Action (On_Vertex : in Vertex);
  procedure Vertex_Enumeration (On_Graph : in Graph);
    -- Traverses the graph ON_GRAPH, executing ACTION for each vertex.

  generic
    with procedure Action (On_Arc : in Arc);
  procedure Arc_Enumeration (On_Graph : in Graph);
    -- Traverses the graph ON_GRAPH, executing ACTION for each arc.


  procedure Assign (Object : in out Graph; Value : in Graph);
    -- Copies VALUE into OBJECT.

  procedure Destroy (The_Graph : in out Graph);
    -- Removes everything from THE_GRAPH.

  procedure Swap (Left, Right : in out Graph);
    -- Exchanges LEFT and RIGHT.


  Vertex_Not_In_Graph,
  No_Vertices_In_Graph,
  No_Arcs_In_Graph       : exception;

private

  type Adj_Arcs_Rec is
    record
      Arriving, Leaving : Arc_Set;
    end record;

  type Adj_Arcs is access Adj_Arcs_Rec;

  package Graph_Table is new Tables(Key_Type  => Vertex,
                                    Item_Type => Adj_Arcs,
                                    "<"       => Smaller,
                                    Count     => Integer);

  type Graph is new Graph_Table.Table;


  pragma Inline(Member, Some_Vertex,
                In_Degree, Out_Degree, Degree,
                Number_Of_Vertices);

end Graph_Handler;
