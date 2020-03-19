with MULTIPLE_KEY_BAGS;

procedure TEST_BAGS_WITH_ZERO_KEYS is
----------------------------------

   type REC is
      record
         A, B, C : FLOAT;
      end record;

   package REC_BAGS is new MULTIPLE_KEY_BAGS(ITEM_TYPE      => REC,
                                             COUNT          => NATURAL,
                                             NUMBER_OF_KEYS => 0);

   A_BAG : REC_BAGS.BAG;

begin
   REC_BAGS.INSERT((A => 0.5, B => 0.1, C => 0.3), INTO => A_BAG);
   REC_BAGS.DESTROY(A_BAG);
   REC_BAGS.FINALIZE;
end TEST_BAGS_WITH_ZERO_KEYS;
