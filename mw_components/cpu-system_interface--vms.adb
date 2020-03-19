with Storage_Units,
     Condition_Handling,
     Starlet,
     System;

separate (CPU)

package body System_Interface is
-----------------------------

   function Process_CPU_Time return System_Ticks is

      JPI_CPU_Time  : System_Ticks;       -- in hundredth of a second

      pragma Volatile(JPI_CPU_Time);

      Return_Status : Condition_Handling.Cond_Value_Type;

      JPI_Item_List : constant Starlet.Item_List_Type :=
         (1 => (Buf_Len     => System.Unsigned_Word(JPI_CPU_Time'Size /
                                                    Storage_Units.Byte_Bits),
                Item_Code   => Starlet.JPI_CPUTim,
                Buf_Address => JPI_CPU_Time'Address,
                Ret_Address => System.Address_Zero),
          2 => (Buf_Len     => 0,
                Item_Code   => 0,
                Buf_Address => System.Address_Zero,
                Ret_Address => System.Address_Zero));

   begin
      Starlet.GetJPIW(Status => Return_Status,
                      Itmlst => JPI_Item_List);
      return JPI_CPU_Time;
   end Process_CPU_Time;


   function Tick_Duration return Duration is
   begin
      return 0.01;
   end;

end System_Interface;
