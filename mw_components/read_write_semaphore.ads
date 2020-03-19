-- READ/WRITE SEMAPHORE PACKAGE
   ----------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Dec  8 16:36:19 1998
-- Update Count    : 18

-- Revision :  8-Dec-1998 by Mats Weber, implemented with a protected type.
-- Revision : 16-Sep-1997 by Mats Weber, removed Set_Priority (too expensive
--                                       to have two select ... else statements
--                                       each time through the loop).
-- Revision :  6-Sep-1988 by Mats Weber, added entries START and STOP.

-- Creation : 15-Aug-1988 by Mats Weber.


with Ada.Finalization;

package Read_Write_Semaphore is
----------------------------

   type Semaphore is limited private;

   type Kind_Of_Operation is (Read, Write);

   type Priority_Holders is (Undefined, Readers, Writers);


   procedure Initialize (The_Semaphore : in Semaphore;
                         Priority_To   : in Priority_Holders := Undefined);
      -- Must be called once and only once to start the semaphore.

   procedure Seize (The_Semaphore : in Semaphore;
                    Operation     : in Kind_Of_Operation);

   procedure Seize_Timed (The_Semaphore : in Semaphore;
                          Operation     : in Kind_Of_Operation;
                          With_Delay    : in Duration;
                          Seized        : out Boolean);

   procedure Seize_Immediate (The_Semaphore : in Semaphore;
                              Operation     : in Kind_Of_Operation;
                              Seized        : out Boolean);

   procedure Release (The_Semaphore : in Semaphore;
                      Operation     : in Kind_Of_Operation);

   procedure Kill (The_Semaphore : in Semaphore);
      -- Although SEMAPHORE has a terminate alternative, this operation
      -- may be used to terminate a SEMAPHORE whose master is a library
      -- package.


   Dead_Semaphore : exception;

private

   type Lock;

   type Lock_Access is access Lock;


   type Semaphore is new Ada.Finalization.Controlled with
      record
         Value : Lock_Access;
            -- Indirection through a pointer because we need the semaphore
            -- to be mode in, and not in out, in Seize, Release, etc.
      end record;

   procedure Initialize (The_Semaphore : in out Semaphore);

   procedure Finalize (The_Semaphore : in out Semaphore);


   pragma Inline (Initialize, Seize, Seize_Timed, Seize_Immediate, Release, Kill);

end Read_Write_Semaphore;
