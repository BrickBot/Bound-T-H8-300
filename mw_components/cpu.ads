-- PACKAGE FOR CPU TIME CALCULATIONS.
   ---------------------------------

-- Creation : 19-SEP-1986 by Mats Weber.


package CPU is
-----------

   function Process_CPU_Time return Duration;
      -- Returns the CPU time used since the process was created.

   type CPU_Counter is private;
      -- Type of a CPU time counter. Each object of this type is an
      -- independant counter.

   procedure Start_Counter (The_Counter : out CPU_Counter);
      -- Starts (or restarts) counter THE_COUNTER.

   procedure Start_Counter;
      -- Restarts the default CPU counter.

   function CPU_Time (Of_Counter : CPU_Counter) return Duration;
      -- Returns the CPU time used since counter OF_COUNTER was started
      -- (or restarted).

   function CPU_Time return Duration;
      -- Returns the CPU time used since the default counter
      -- was started (or restarted).

   Counter_Not_Started : exception;

private

   type System_Ticks is range 0 .. 2 ** 31 - 1;

   type CPU_Counter is
      record
         Started    : Boolean := False;
         Start_Time : System_Ticks;
      end record;

end CPU;
