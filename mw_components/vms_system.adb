-- INTERFACE TO VMS SYSTEM ROUTINES
   --------------------------------

-- Creation :  9-NOV-1989 by Mats Weber.


with Condition_Handling,
     Starlet;

use Condition_Handling,
    Starlet;

package body VMS_System is
-----------------------

   procedure Check_Condition (Status : in Condition_Handling.Cond_Value_Type) is
   begin
      if not Condition_Handling.Success(Status) then
         Condition_Handling.Signal(Status);
      end if;
   end Check_Condition;


   procedure Set_Process_Name (To : in String) is

      Return_Status : Condition_Handling.Cond_Value_Type;

   begin
      Starlet.SetPrN(Status => Return_Status,
                     PrcNam => To);
      Check_Condition(Return_Status);
   end Set_Process_Name;

end VMS_System;
