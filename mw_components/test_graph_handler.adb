with TEXT_IO,
     INTEGER_TEXT_IO,
     GRAPH_HANDLER,
     DIRECTED_GRAPH_OPERATIONS,
     NONDIRECTED_GRAPH_OPERATIONS,
     DYNAMIC_ARRAYS;

use TEXT_IO,
    INTEGER_TEXT_IO,
    GRAPH_HANDLER,
    DIRECTED_GRAPH_OPERATIONS,
    NONDIRECTED_GRAPH_OPERATIONS;

procedure TEST_GRAPH_HANDLER is
          ------------------

  use GRAPH_HANDLER.SET_OF_VERTEX,
      GRAPH_HANDLER.SET_OF_ARC;

  type STRING_ACCESS is access STRING;

  package VERTEX_NAME_ARRAYS is new DYNAMIC_ARRAYS(VERTEX, STRING_ACCESS, SMALLER, INTEGER);
  use VERTEX_NAME_ARRAYS;

  N : constant := 17;
  V : array (1..N) of VERTEX;
  G : GRAPH;

  NAMES : VERTEX_NAME_ARRAYS.DYNAMIC_ARRAY;


  function NAME (V : VERTEX) return STRING is
  begin
    return COMPONENT(NAMES, INDEX => V).all;
  end NAME;


  procedure BUILD_GRAPH is
  begin
    declare
      ST : STRING(1..2);
    begin
      for I in 1..N loop
        PUT(ST,I);
        for J in ST'RANGE loop
          if ST(J) = ' ' then
            ST(J) := '-';
          end if;
        end loop;
        V(I) := NEW_VERTEX;
        ASSIGN(OBJECT => NAMES, INDEX => V(I), VALUE => new STRING'("V" & ST));
        ADD(V(I),G);
      end loop;
    end;
    ADD(NEW_ARC(V(1),V(1)),G);
    ADD(NEW_ARC(V(1),V(3)),G);
    ADD(NEW_ARC(V(1),V(4)),G);
    ADD(NEW_ARC(V(2),V(3)),G);
    ADD(NEW_ARC(V(4),V(2)),G);
    ADD(NEW_ARC(V(5),V(1)),G);
    ADD(NEW_ARC(V(5),V(4)),G);
    ADD(NEW_ARC(V(5),V(6)),G);
    ADD(NEW_ARC(V(5),V(8)),G);
    ADD(NEW_ARC(V(6),V(7)),G);
    ADD(NEW_ARC(V(7),V(6)),G);
    ADD(NEW_ARC(V(7),V(6)),G);
    ADD(NEW_ARC(V(7),V(8)),G);
    ADD(NEW_ARC(V(8),V(5)),G);
    ADD(NEW_ARC(V(8),V(9)),G);
    ADD(NEW_ARC(V(9),V(8)),G);
    ADD(NEW_ARC(V(10),V(9)),G);
    ADD(NEW_ARC(V(10),V(11)),G);
    ADD(NEW_ARC(V(11),V(2)),G);
    ADD(NEW_ARC(V(11),V(3)),G);
    ADD(NEW_ARC(V(11),V(4)),G);
    ADD(NEW_ARC(V(11),V(8)),G);
    ADD(NEW_ARC(V(11),V(9)),G);
    ADD(NEW_ARC(V(11),V(10)),G);
    ADD(NEW_ARC(V(11),V(11)),G);

    ADD(NEW_ARC(V(12),V(12)),G);

    ADD(NEW_ARC(V(14),V(15)),G);
    ADD(NEW_ARC(V(15),V(14)),G);

    ADD(NEW_ARC(V(16),V(16)),G);
    ADD(NEW_ARC(V(16),V(16)),G);
    ADD(NEW_ARC(V(16),V(17)),G);
  end BUILD_GRAPH;


  procedure TEST_PRED_SUCC is

    procedure PUT_PREDECESSORS (V : in VERTEX) is

      P : constant SET_OF_VERTEX.LIST := PRED(V,G);

    begin
      PUT("Predecessors of "); PUT(NAME(V));
      NEW_LINE;
      for I in P'RANGE loop
        PUT("  ");
        PUT(NAME(P(I)));
      end loop;
      NEW_LINE;
    end;

    procedure PUT_ALL_PREDECESSORS is new VERTEX_ENUMERATION(PUT_PREDECESSORS);


    procedure PUT_SUCCESSORS (V : in VERTEX) is

      P : constant SET_OF_VERTEX.LIST := SUCC(V,G);

    begin
      PUT("Successors of "); PUT(NAME(V));
      NEW_LINE;
      for I in P'RANGE loop
        PUT("  ");
        PUT(NAME(P(I)));
      end loop;
      NEW_LINE;
    end;

    procedure PUT_ALL_SUCCESSORS is new VERTEX_ENUMERATION(PUT_SUCCESSORS);


    procedure PUT_ADJACENT (V : in VERTEX) is

      P : constant SET_OF_VERTEX.LIST := ADJACENT(V,G);

    begin
      PUT("Vertices adjacent to "); PUT(NAME(V));
      NEW_LINE;
      for I in P'RANGE loop
        PUT("  ");
        PUT(NAME(P(I)));
      end loop;
      NEW_LINE;
    end;

    procedure PUT_ALL_ADJACENT is new VERTEX_ENUMERATION(PUT_ADJACENT);

  begin
    PUT_ALL_PREDECESSORS(G);
    PUT_ALL_SUCCESSORS(G);
    PUT_ALL_ADJACENT(G);
  end TEST_PRED_SUCC;


  procedure TEST_ARRIVING_LEAVING is

    procedure PUT_ARRIVING (V : in VERTEX) is

      A : constant SET_OF_ARC.LIST := ARRIVING(V,G);

    begin
      PUT("Arcs arriving to "); PUT(NAME(V));
      NEW_LINE;
      for I in A'RANGE loop
        PUT("  ");
        PUT("(");
        PUT(NAME(INITIAL(A(I)))); PUT(",");
        PUT(NAME(FINAL(A(I))));
        PUT(")");
      end loop;
      NEW_LINE;
    end;

    procedure PUT_ALL_ARRIVING is new VERTEX_ENUMERATION(PUT_ARRIVING);


    procedure PUT_ADJACENT (V : in VERTEX) is

      A : constant SET_OF_ARC.LIST := ADJACENT(V,G);

    begin
      PUT("Arcs adjacent to "); PUT(NAME(V));
      NEW_LINE;
      for I in A'RANGE loop
        PUT("  ");
        PUT("(");
        PUT(NAME(INITIAL(A(I)))); PUT(",");
        PUT(NAME(FINAL(A(I))));
        PUT(")");
      end loop;
      NEW_LINE;
    end;

    procedure PUT_ALL_ADJACENT is new VERTEX_ENUMERATION(PUT_ADJACENT);

  begin
    PUT_ALL_ARRIVING(G);
    PUT_ALL_ADJACENT(G);
  end TEST_ARRIVING_LEAVING;


  procedure TEST_DEGREES is

    procedure PUT_DEGREES (V : in VERTEX) is
    begin
      PUT("Degrees of "); PUT(NAME(V));
      NEW_LINE;
      PUT("in :"); PUT(IN_DEGREE(V,G), WIDTH => 3);
      PUT("    out :"); PUT(OUT_DEGREE(V,G), WIDTH => 3);
      PUT("    in-out :"); PUT(DEGREE(V,G), WIDTH => 3);
      NEW_LINE;
    end;

    procedure PUT_ALL_DEGREES is new VERTEX_ENUMERATION(PUT_DEGREES);

  begin
    PUT_ALL_DEGREES(G);
  end TEST_DEGREES;


  procedure TEST_TRAVERSALS is

     procedure PUT_NAME (V : in VERTEX) is
     begin
        PUT("  " & NAME(V));
     end PUT_NAME;

     procedure TRAVERSE is new DIRECTED_GRAPH_OPERATIONS.GRAPH_TRAVERSAL(ACTION => PUT_NAME);
     procedure TRAVERSE is new NONDIRECTED_GRAPH_OPERATIONS.GRAPH_TRAVERSAL(ACTION => PUT_NAME);

  begin
     PUT_LINE("TRAVERSE(FROM => V(7), IN_GRAPH => G, ORDER => DEPTH_FIRST);");
     TRAVERSE(FROM => V(7), IN_GRAPH => G, ORDER => DEPTH_FIRST);
     NEW_LINE;
     PUT_LINE("TRAVERSE(FROM => V(7), IN_GRAPH => G, ORDER => BREADTH_FIRST);");
     TRAVERSE(FROM => V(7), IN_GRAPH => G, ORDER => BREADTH_FIRST);
     NEW_LINE;
     PUT_LINE("TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => DEPTH_FIRST, DIRECTION => FORWARD);");
     TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => DEPTH_FIRST, DIRECTION => FORWARD);
     NEW_LINE;
     PUT_LINE("TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => BREADTH_FIRST, DIRECTION => FORWARD);");
     TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => BREADTH_FIRST, DIRECTION => FORWARD);
     NEW_LINE;
     PUT_LINE("TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => DEPTH_FIRST, DIRECTION => BACKWARD);");
     TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => DEPTH_FIRST, DIRECTION => BACKWARD);
     NEW_LINE;
     PUT_LINE("TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => BREADTH_FIRST, DIRECTION => BACKWARD);");
     TRAVERSE(FROM => V(8), IN_GRAPH => G, ORDER => BREADTH_FIRST, DIRECTION => BACKWARD);
     NEW_LINE;
  end TEST_TRAVERSALS;


begin
  INTEGER_TEXT_IO.DEFAULT_WIDTH := 0;
  BUILD_GRAPH;
  TEST_PRED_SUCC;
  TEST_ARRIVING_LEAVING;
  TEST_DEGREES;
  TEST_TRAVERSALS;
  DESTROY(G);
end TEST_GRAPH_HANDLER;
