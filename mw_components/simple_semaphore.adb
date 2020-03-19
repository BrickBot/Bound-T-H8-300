-- SIMPLE SEMAPHORE PACKAGE
   ------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Dec  8 16:30:59 1998
-- Update Count    : 24

-- Creation : 12-AUG-1988 by Mats Weber.


with Ada.Unchecked_Deallocation;

package body Simple_Semaphore is
-----------------------------

   protected type Lock is

      entry Seize;
      procedure Release;
      entry Kill;

   private

      Busy, Dead : Boolean := False;

   end Lock;


   procedure Initialize (The_Semaphore : in out Semaphore) is
   begin
      The_Semaphore.Value := new Lock;
   end Initialize;


   procedure Finalize (The_Semaphore : in out Semaphore) is

      procedure Dispose is new Ada.Unchecked_Deallocation(Lock, Lock_Access);

   begin
      Dispose(The_Semaphore.Value);
   end Finalize;


   procedure Seize (The_Semaphore : in Semaphore) is
   begin
      The_Semaphore.Value.Seize;
   end Seize;


   procedure Seize_Timed (The_Semaphore : in Semaphore;
                          With_Delay    : in Duration;
                          Seized        : out Boolean) is
   begin
      select
         The_Semaphore.Value.Seize;
         Seized := True;
      or
         delay With_Delay;
         Seized := False;
      end select;
   end Seize_Timed;


   procedure Seize_Immediate (The_Semaphore : in Semaphore;
                              Seized        : out Boolean) is
   begin
      select
         The_Semaphore.Value.Seize;
         Seized := True;
      else
         Seized := False;
      end select;
   end Seize_Immediate;


   procedure Release (The_Semaphore : in Semaphore) is
   begin
      The_Semaphore.Value.Release;
   end Release;


   procedure Kill (The_Semaphore : in Semaphore) is
   begin
      The_Semaphore.Value.Kill;
   end Kill;


   Illegal_Release : exception;  -- Declared here because it is not
                                 -- intended to be used outside.


   protected body Lock is

      entry Seize when not Busy is
      begin
         if Dead then
            raise Dead_Semaphore;
         end if;
         Busy := True;
      end Seize;

      procedure Release is
      begin
         if Dead then
            raise Dead_Semaphore;
         end if;
         if not Busy then
            raise Illegal_Release;
         end if;
         Busy := False;
      end Release;

      entry Kill when not Busy is
      begin
         if Dead then
            raise Dead_Semaphore;
         end if;
         Dead := True;
      end Kill;

   end Lock;

end Simple_Semaphore;
