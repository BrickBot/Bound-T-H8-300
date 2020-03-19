with System;

separate (CPU)

package body System_Interface is
-----------------------------

   function Process_CPU_Time return System_Ticks is

      type UNIX_Times is
         record
            User_Time,
            System_Time,
            Children_User_Time,
            Children_System_Time : System_Ticks;
         end record;

      procedure Times (Times_Rec : in System.Address);

      pragma Interface (C, Times);

      Process_Times : UNIX_Times;

   begin
      Times(Process_Times'Address);
      return Process_Times.User_Time + Process_Times.System_Time;
   end Process_CPU_Time;


   function Tick_Duration return Duration is
   begin
      return 0.01;
   end;

end System_Interface;
