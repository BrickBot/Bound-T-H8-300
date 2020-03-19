with Tables;

procedure Test_Tables is
---------------------

   Test_Error : exception;

   subtype String_10 is String(1..10);

   type Natural_Array is array (Positive range <>) of Natural;

   type Info is
      record
         Number      : Natural;
         Occurences  : Natural_Array(1..20) := (others => 0);
      end record;

   package Symbol_Table is new Tables(Key_Type  => String_10,
                                      Item_Type => Info,
                                      Count     => Natural);
   use Symbol_Table;

   T : Table;

begin
   Insert(Key => "Key 1     ", Item => (1, (3, 4, 7, others => 0)), Into => T);
   Insert(Key => "Key 3     ", Item => (1, (7, 7, 1, others => 0)), Into => T);
   Insert(Key => "Key 7     ", Item => (1, (2, 3, 4, others => 0)), Into => T);
   Insert(Key => "Key 2     ", Item => (1, (3, 2, 2, others => 0)), Into => T);
   Insert(Key => "Key 5     ", Item => (1, (5, 7, 9, others => 0)), Into => T);
   begin
      Insert(Key => "Key 3     ", Item => (1, (7, 7, 1, others => 0)), Into => T);
      raise Test_Error;
   exception
      when Duplicate_Key =>
         null;
   end;
end Test_Tables;
