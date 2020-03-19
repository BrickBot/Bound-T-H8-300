with GRAPH_HANDLER,
     MATCHINGS,
     TABLES,
     NUMBER_IMAGES,
     TEXT_IO;

use GRAPH_HANDLER,
    MATCHINGS,
    TEXT_IO;

procedure TEST_MATCHINGS is

   type VERTEX_NAME is range 1..8;

   package VERTEX_TO_VERTEX_NAME_TABLES is new TABLES(KEY_TYPE  => VERTEX,
                                                      ITEM_TYPE => VERTEX_NAME,
                                                      "<"       => SMALLER,
                                                      COUNT     => NATURAL);

   use VERTEX_TO_VERTEX_NAME_TABLES;

   VERTICES : array (VERTEX_NAME) of VERTEX := (others => NEW_VERTEX);
   NAMES    : VERTEX_TO_VERTEX_NAME_TABLES.TABLE;

   G        : GRAPH;

   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(VERTEX_NAME);

begin
   for NAME in VERTICES'RANGE loop
      INSERT(KEY => VERTICES(NAME), ITEM => NAME, INTO => NAMES);
   end loop;
   ADD(VERTEX_LIST(VERTICES), TO => G);
   ADD(NEW_ARC(VERTICES(1), VERTICES(2)), TO => G);
   ADD(NEW_ARC(VERTICES(2), VERTICES(3)), TO => G);
   ADD(NEW_ARC(VERTICES(3), VERTICES(4)), TO => G);
   ADD(NEW_ARC(VERTICES(4), VERTICES(5)), TO => G);
   ADD(NEW_ARC(VERTICES(5), VERTICES(6)), TO => G);
   ADD(NEW_ARC(VERTICES(6), VERTICES(7)), TO => G);
   ADD(NEW_ARC(VERTICES(1), VERTICES(3)), TO => G);
   ADD(NEW_ARC(VERTICES(2), VERTICES(5)), TO => G);
   ADD(NEW_ARC(VERTICES(4), VERTICES(7)), TO => G);
   ADD(NEW_ARC(VERTICES(6), VERTICES(8)), TO => G);
   declare

      M : constant ARC_LIST := MAXIMUM_CARDINALITY_MATCHING(G);

   begin
      PUT_LINE("Maximum matching on G :");
      for I in M'RANGE loop
         PUT_LINE("   (" & IMAGE(SEARCH(INITIAL(M(I)), NAMES)) & "," &
                           IMAGE(SEARCH(FINAL(M(I)),   NAMES)) & ")");
      end loop;
   end;
end TEST_MATCHINGS;
