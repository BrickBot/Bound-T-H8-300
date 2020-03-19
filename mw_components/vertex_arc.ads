-- DEFINITION OF TYPES VERTEX AND ARC FOR GRAPH_HANDLER
   ----------------------------------------------------

-- Revision : 15-APR-1987 by Mats Weber, use package KEYS for identifying vertices and arcs.
-- Revision :  7-DEC-1987 by Mats Weber, added function OTHER_END(ARC, VERTEX) return VERTEX.
-- Revision : 11-NOV-1987 by Mats Weber, stop using access types for
--                                       VERTEX and ARC.

-- Creation : 14-NOV-1985 by Mats Weber.


with Keys;

package Vertex_Arc is
------------------

  type Vertex is private;
  -----------

  function New_Vertex return Vertex;
    -- Creates a new vertex.
    -- Raises TOO_MANY_VERTICES if no more vertices can be created.

  function Smaller (V1, V2 : Vertex) return Boolean;
    -- Ordering function on vertices.

  procedure Check (The_Vertex : in Vertex);
    -- Checks if THE_VERTEX has been initialized and
    -- raises UNINITIALIZED_VERTEX if not.

  Uninitialized_Vertex,
  Too_Many_Vertices      : exception;


  type Arc is private;
  --------

  function New_Arc (Initial, Final : Vertex) return Arc;
    -- Creates a new arc.
    -- Raises TOO_MANY_ARCS if no more arcs can be created.

  function Initial (Of_Arc : Arc) return Vertex;
    -- Returns the initial vertex of OF_ARC.

  function Final (Of_Arc : Arc) return Vertex;
    -- Returns the final vertex of OF_ARC.

  function Other_End (Of_Arc : Arc; This_End : Vertex) return Vertex;
    -- Returns FINAL(OF_ARC) if THIS_END = INITIAL(OF_ARC),
    --         INITIAL(OF_ARC) if THIS_END = FINAL(OF_ARC),
    -- raises VERTEX_NOT_ON_ARC otherwise.

  function Smaller (A1, A2 : Arc) return Boolean;
    -- Ordering function on arcs.

  procedure Check (The_Arc : in Arc);
    -- Checks if A has been initialized and
    -- raises UNINITIALIZED_ARC if not.

  Uninitialized_Arc,
  Too_Many_Arcs,
  Vertex_Not_On_Arc   : exception;

private

  package Vertex_Keys is new Keys;

  type Vertex is new Vertex_Keys.Key;

  package Arc_Keys is new Keys;

  type Arc is
    record
      Signature : Arc_Keys.Key;
      Initial,
      Final     : Vertex;
    end record;


  pragma Inline(New_Vertex, New_Arc, Smaller, Initial, Final, Other_End, Check);

end Vertex_Arc;
