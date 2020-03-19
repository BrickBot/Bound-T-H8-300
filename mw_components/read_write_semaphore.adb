-- READ/WRITE SEMAPHORE PACKAGE
   ----------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Dec  8 16:42:27 1998
-- Update Count    : 30

-- Revision : 30-Mar-1989 by Mats Weber, corrected the "stowaway reader" problem
--                                       seen by A. Schiper.

-- Creation : 15-Aug-1988 by Mats Weber.


with Ada.Unchecked_Deallocation;

package body Read_Write_Semaphore is
---------------------------------

   protected type Lock is

      procedure Initialize (Priority_To : in Priority_Holders);

      entry Seize(Kind_Of_Operation);
      entry Release(Kind_Of_Operation);

      entry Kill;

   private

      Initialized       : Boolean := False;
      Number_Of_Readers : Natural := 0;
      Write_In_Progress : Boolean := False;
      Priority          : Priority_Holders;
      Dead              : Boolean := False;

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


   procedure Initialize (The_Semaphore : in Semaphore;
                         Priority_To   : in Priority_Holders := Undefined) is
   begin
      The_Semaphore.Value.Initialize(Priority_To);
   end Initialize;


   procedure Seize (The_Semaphore : in Semaphore;
                    Operation     : in Kind_Of_Operation) is
   begin
      The_Semaphore.Value.Seize(Operation);
   end Seize;


   procedure Seize_Timed (The_Semaphore : in Semaphore;
                          Operation     : in Kind_Of_Operation;
                          With_Delay    : in Duration;
                          Seized        : out Boolean) is
   begin
      select
         The_Semaphore.Value.Seize(Operation);
         Seized := True;
      or
         delay With_Delay;
         Seized := False;
      end select;
   end Seize_Timed;


   procedure Seize_Immediate (The_Semaphore : in Semaphore;
                              Operation     : in Kind_Of_Operation;
                              Seized        : out Boolean) is
   begin
      select
         The_Semaphore.Value.Seize(Operation);
         Seized := True;
      else
         Seized := False;
      end select;
   end Seize_Immediate;


   procedure Release (The_Semaphore : in Semaphore;
                      Operation     : in Kind_Of_Operation) is
   begin
      The_Semaphore.Value.Release(Operation);
   end Release;


   procedure Kill (The_Semaphore : in Semaphore) is
   begin
      The_Semaphore.Value.Kill;
   end Kill;


   Illegal_Release : exception;  -- Declared here because it is not
                                 -- intended to be used outside.


   protected body Lock is

      procedure Initialize (Priority_To : in Priority_Holders) is
      begin
         Initialized := True;
         Priority := Priority_To;
      end Initialize;

      entry Seize(for Operation in Kind_Of_Operation)
         when Initialized and
              ((Operation = Read and not Write_In_Progress) or
               (Operation = Write and not Write_In_Progress and Number_Of_Readers = 0)) is
      begin
         if Dead then
            raise Dead_Semaphore;
         end if;
         case Operation is
            when Read =>
               Number_Of_Readers := Number_Of_Readers + 1;
            when Write =>
               Write_In_Progress := True;
         end case;
      end Seize;

      entry Release(for Operation in Kind_Of_Operation) when True is
      begin
         if Dead then
            raise Dead_Semaphore;
         end if;
         case Operation is
            when Read =>
               if Number_Of_Readers = 0 then
                  raise Illegal_Release;
               end if;
               Number_Of_Readers := Number_Of_Readers - 1;
            when Write =>
               if not Write_In_Progress then
                  raise Illegal_Release;
               end if;
               Write_In_Progress := False;
         end case;
      end Release;

      entry Kill
         when Initialized and not Write_In_Progress and Number_Of_Readers = 0 is
      begin
         if Dead then
            raise Dead_Semaphore;
         end if;
         Dead := True;
      end Kill;

   end Lock;

end Read_Write_Semaphore;
