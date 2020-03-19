-- SIMPLE SEMAPHORE PACKAGE
   ------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Dec  8 16:36:20 1998
-- Update Count    : 17

-- Revision :  8-Dec-1998 by Mats Weber, implemented with a protected type.
-- Revision :  6-SEP-1988 by Mats Weber, added entry KILL.

-- Creation : 12-AUG-1988 by Mats Weber.


with Ada.Finalization;

package Simple_Semaphore is
------------------------

   type Semaphore is limited private;


   procedure Seize (The_Semaphore : in Semaphore);

   procedure Seize_Timed (The_Semaphore : in Semaphore;
                          With_Delay    : in Duration;
                          Seized        : out Boolean);

   procedure Seize_Immediate (The_Semaphore : in Semaphore;
                              Seized        : out Boolean);

   procedure Release (The_Semaphore : in Semaphore);

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


   pragma Inline (Seize, Release, Kill);

end Simple_Semaphore;
