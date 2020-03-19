-- PACKAGE FOR CPU TIME CALCULATIONS.
   ---------------------------------

-- Revision : 16-Jul-1992 by Mats Weber, enhanced portability by adding
--                                       separate package System_Interface.

-- Creation : 19-SEP-1986 by Mats Weber.


package body CPU is
----------------

   Default_Counter : CPU_Counter;


   package System_Interface is
   ------------------------

      function Process_CPU_Time return System_Ticks;

      function Tick_Duration return Duration;

      pragma Inline (Tick_Duration);

   end System_Interface;

   package body System_Interface is separate;


   function Process_CPU_Time return Duration is
   begin
      return Integer(System_Interface.Process_CPU_Time) * System_Interface.Tick_Duration;
   end;


   procedure Start_Counter (The_Counter : out CPU_Counter) is
   begin
      The_Counter := (Started    => True,
                      Start_Time => System_Interface.Process_CPU_Time);
   end;


   procedure Start_Counter is
   begin
      Start_Counter(Default_Counter);
   end;


   function CPU_Time (Of_Counter : CPU_Counter) return Duration is
   begin
      if Of_Counter.Started then
         return Integer(System_Interface.Process_CPU_Time - Of_Counter.Start_Time) *
                System_Interface.Tick_Duration;
      else
         raise Counter_Not_Started;
      end if;
   end CPU_Time;


   function CPU_Time return Duration is
   begin
      return CPU_Time(Default_Counter);
   end;

begin
   Start_Counter;  -- Start the default counter.
end CPU;
