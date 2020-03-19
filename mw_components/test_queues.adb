-- Filename        : test_queues.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jun 24 15:05:46 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jun 24 15:09:38 1998
-- Update Count    : 7


with Queues,
     Text_IO,
     Integer_Text_IO;

use Text_IO,
    Integer_Text_IO;

procedure Test_Queues is

   package Int_Queues is new Queues(Integer, Count => Natural);
   use Int_Queues;

   type Action is (Display, Put, Get);

   Ch      : Character;
   Do_What : Action;

   procedure Display (Q : in Queue) is

      procedure Display_Element (X : in Integer) is
      begin
         Put(N, 5); Put(" : ");
         Put(X, 7);
      end;

      procedure Print_Queue is new Traversal(Display_Element);

   begin
      Print_Queue(Q);
   end Display;

begin
   loop
      Put("D : display, P : put, G : get >>");
      Get(Ch);
      case Ch is
         when 'D' | 'd' =>
            Display(Q1);
         when 'P' | 'p' =>
            Put("N : ");
            Get(N);
            Put(N, Q1);
         when others =>
            null;
      end case;
   end loop;
end Test_Queues;
