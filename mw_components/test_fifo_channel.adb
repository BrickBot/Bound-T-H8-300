with FIFO_CHANNEL,
     SIMPLE_SEMAPHORE,
     NUMBER_IMAGES,
     TEXT_IO;

procedure TEST_FIFO_CHANNEL is
---------------------------

   N_MESSAGES : constant := 5;

   task T1 is
      entry RECEIVE (MESSAGE : in CHARACTER);
   end T1;

   task T2 is
      entry RECEIVE (MESSAGE : in INTEGER);
      entry RECEIVE (MESSAGE : in CHARACTER);
   end T2;

   TEXT_IO_SEMAPHORE : SIMPLE_SEMAPHORE.SEMAPHORE;

   procedure PUT_LINE (ITEM : in STRING);

   function IMAGE is new NUMBER_IMAGES.INTEGER_IMAGE(INTEGER);


   task body T1 is

      package CHANNEL_FROM_T1_TO_T2 is new FIFO_CHANNEL;

      package INTEGER_CHANNEL_FROM_T1_TO_T2 is
         new CHANNEL_FROM_T1_TO_T2.SENDER(MESSAGE_TYPE => INTEGER,
                                          DESTINATION  => T2.RECEIVE);

      package CHARACTER_CHANNEL_FROM_T1_TO_T2 is
         new CHANNEL_FROM_T1_TO_T2.SENDER(MESSAGE_TYPE => CHARACTER,
                                          DESTINATION  => T2.RECEIVE);

   begin
      for I in 1..N_MESSAGES loop
         accept RECEIVE (MESSAGE : in CHARACTER) do
            PUT_LINE("T1 receives '" & MESSAGE & ''');
         end RECEIVE;
         INTEGER_CHANNEL_FROM_T1_TO_T2.SEND(MESSAGE => I);
         PUT_LINE("T1 sent " & IMAGE(I) & " to T2");
         CHARACTER_CHANNEL_FROM_T1_TO_T2.SEND(MESSAGE =>
                                                 CHARACTER'VAL(CHARACTER'POS('A') + (I - 1) mod 26));
         PUT_LINE("T1 sent '" & CHARACTER'VAL(CHARACTER'POS('A') + (I - 1) mod 26) & "' to T2");
      end loop;
   end T1;


   task body T2 is
   begin
      for I in INTEGER range 1..2 * N_MESSAGES loop
         select
            accept RECEIVE (MESSAGE : in INTEGER) do
               PUT_LINE("T2 receives " & IMAGE(MESSAGE));
            end RECEIVE;
         or
            accept RECEIVE (MESSAGE : in CHARACTER) do
               PUT_LINE("T2 receives '" & MESSAGE & ''');
            end RECEIVE;
         end select;
      end loop;
   end T2;


   procedure PUT_LINE (ITEM : in STRING) is
   begin
      TEXT_IO_SEMAPHORE.SEIZE;
      TEXT_IO.PUT_LINE(ITEM);
      TEXT_IO_SEMAPHORE.RELEASE;
   end PUT_LINE;


begin
   declare

      package CHANNEL_FROM_MAIN_TO_T1 is new FIFO_CHANNEL;

      package CHARACTER_CHANNEL_FROM_MAIN_TO_T1 is
         new CHANNEL_FROM_MAIN_TO_T1.SENDER(MESSAGE_TYPE => CHARACTER,
                                            DESTINATION  => T1.RECEIVE);

   begin
      for I in 1..N_MESSAGES loop
         CHARACTER_CHANNEL_FROM_MAIN_TO_T1.SEND(MESSAGE =>
                                                   CHARACTER'VAL(CHARACTER'POS('A') + 25 - (I - 1) mod 26));
         PUT_LINE("MAIN sent '" & CHARACTER'VAL(CHARACTER'POS('A') + 25 - (I - 1) mod 26) & "' to T1");
      end loop;
   end;
end TEST_FIFO_CHANNEL;
