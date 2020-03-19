with READ_WRITE_SEMAPHORE,
     CALENDAR,
     NUMBER_IMAGES,
     TEXT_IO,
     USER_INTERFACE;

use READ_WRITE_SEMAPHORE,
    CALENDAR;

procedure TEST_READ_WRITE_SEMAPHORE is
-----------------------------------

   pragma TIME_SLICE(0.01);

   S : READ_WRITE_SEMAPHORE.SEMAPHORE;

   task type READER is
      entry START;
      entry GET_MAX_WAIT_TIME (MAX_WAIT_TIME : out DURATION);
   end READER;

   task type WRITER is
      entry START;
      entry GET_MAX_WAIT_TIME (MAX_WAIT_TIME : out DURATION);
   end WRITER;


   function IMAGE is new NUMBER_IMAGES.FIXED_IMAGE(DURATION);
   function NATURAL_ANSWER is new USER_INTERFACE.INTEGER_ANSWER(NATURAL);
   function DURATION_ANSWER is new USER_INTERFACE.FIXED_ANSWER(DURATION);


   ALL_READERS : array (1..NATURAL_ANSWER("Number of readers : ")) of READER;
   ALL_WRITERS : array (1..NATURAL_ANSWER("Number of writers : ")) of WRITER;

   READER_SEIZE_TIME : constant DURATION := DURATION_ANSWER("Reader Seize Time : ");
   READER_WAIT_TIME  : constant DURATION := DURATION_ANSWER("Reader Wait Time  : ");
   WRITER_SEIZE_TIME : constant DURATION := DURATION_ANSWER("Writer Seize Time : ");
   WRITER_WAIT_TIME  : constant DURATION := DURATION_ANSWER("Writer Wait Time  : ");
   EXECUTION_TIME    : constant DURATION := DURATION_ANSWER("Time in seconds to run the program : ");


   task body READER is

      WAIT_TIME     : DURATION;
      MAX_WAIT_TIME : DURATION := 0.0;
      CALL_TIME     : CALENDAR.TIME;

   begin
      accept START;
      loop
         CALL_TIME := CALENDAR.CLOCK;
         S.SEIZE(READ);
         WAIT_TIME := CALENDAR.CLOCK - CALL_TIME;
         if WAIT_TIME > MAX_WAIT_TIME then
            MAX_WAIT_TIME := WAIT_TIME;
         end if;
         delay READER_SEIZE_TIME;
         S.RELEASE(READ);
         select
            accept GET_MAX_WAIT_TIME (MAX_WAIT_TIME : out DURATION) do
               MAX_WAIT_TIME := READER.MAX_WAIT_TIME;
            end GET_MAX_WAIT_TIME;
            exit;
         or
            delay READER_WAIT_TIME;
         end select;
      end loop;
   end READER;


   task body WRITER is

      WAIT_TIME     : DURATION;
      MAX_WAIT_TIME : DURATION := 0.0;
      CALL_TIME     : CALENDAR.TIME;

   begin
      accept START;
      loop
         CALL_TIME := CALENDAR.CLOCK;
         S.SEIZE(WRITE);
         WAIT_TIME := CALENDAR.CLOCK - CALL_TIME;
         if WAIT_TIME > MAX_WAIT_TIME then
            MAX_WAIT_TIME := WAIT_TIME;
         end if;
         delay WRITER_SEIZE_TIME;
         S.RELEASE(WRITE);
         select
            accept GET_MAX_WAIT_TIME (MAX_WAIT_TIME : out DURATION) do
               MAX_WAIT_TIME := WRITER.MAX_WAIT_TIME;
            end GET_MAX_WAIT_TIME;
            exit;
         or
            delay WRITER_WAIT_TIME;
         end select;
      end loop;
   end WRITER;


   function PRIORITY_ANSWER (PROMPT : STRING) return READ_WRITE_SEMAPHORE.PRIORITY_HOLDERS is
   begin
      loop
         declare

            ANSWER : constant STRING := USER_INTERFACE.STRING_ANSWER(PROMPT);

         begin
            if ANSWER'LENGTH = 1 then
               case ANSWER(ANSWER'FIRST) is
                  when 'U' | 'u' =>
                     return UNDEFINED;
                  when 'R' | 'r' =>
                     return READERS;
                  when 'W' | 'w' =>
                     return WRITERS;
                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;
   end PRIORITY_ANSWER;

begin
   S.INITIALIZE(PRIORITY_TO => PRIORITY_ANSWER("Priority (U : undefined, R : readers, W : writers) : "));
   declare

      I : NATURAL := 0;

   begin
      while I < ALL_READERS'LAST or I < ALL_WRITERS'LAST loop
         I := I + 1;
         if I <= ALL_READERS'LAST then
            ALL_READERS(I).START;
         end if;
         if I <= ALL_WRITERS'LAST then
            ALL_WRITERS(I).START;
         end if;
      end loop;
   end;
   delay EXECUTION_TIME;
   S.SET_PRIORITY(TO => WRITERS);
   declare

      MAX_READ_WAIT_TIME,
      MAX_WRITE_WAIT_TIME  : DURATION := 0.0;

      TASK_MAX_TIME        : DURATION;

      I                    : NATURAL := 0;

   begin
      while I < ALL_READERS'LAST or I < ALL_WRITERS'LAST loop
         I := I + 1;
         if I <= ALL_READERS'LAST then
            ALL_READERS(I).GET_MAX_WAIT_TIME(TASK_MAX_TIME);
            if TASK_MAX_TIME > MAX_READ_WAIT_TIME then
               MAX_READ_WAIT_TIME := TASK_MAX_TIME;
            end if;
         end if;
         if I <= ALL_WRITERS'LAST then
            ALL_WRITERS(I).GET_MAX_WAIT_TIME(TASK_MAX_TIME);
            if TASK_MAX_TIME > MAX_WRITE_WAIT_TIME then
               MAX_WRITE_WAIT_TIME := TASK_MAX_TIME;
            end if;
         end if;
      end loop;
      TEXT_IO.PUT_LINE("Maximum reader wait time : " & IMAGE(MAX_READ_WAIT_TIME));
      TEXT_IO.PUT_LINE("Maximum writer wait time : " & IMAGE(MAX_WRITE_WAIT_TIME));
   end;
end TEST_READ_WRITE_SEMAPHORE;
