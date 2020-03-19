-- GENERIC FIFO BUFFER TASK TYPE
   -----------------------------

-- Revision : 11-Feb-1992 by Mats Weber, added terminate and Kill
--                                       alternatives when Set_Size
--                                       can be called.

-- Creation : 21-MAR-1989 by Mats Weber.


with Queues;

package body Buffers is
--------------------

   task body Buffer is
   ----------------

      Size : Natural_Count;
      Dead : Boolean := False;

   begin
      select
         accept Set_Size (To : in Natural_Count) do
            Size := To;
         end;
      or
         accept Kill;
         Dead := True;
      or
         terminate;
      end select;
      --
      if not Dead then
         if Size = Unbounded then
            declare

               package Item_Queues is new Queues(Item_Type,
                                                 Count => Natural_Count);

               Pool : Item_Queues.Queue;

               use Item_Queues;

            begin
               loop
                  select
                     accept Write (Item : in Item_Type) do
                        Put(Item => Item, Into => Pool);
                     end;
                  or
                     when not Empty(Pool) =>
                        accept Read (Item : out Item_Type) do
                           Get(Item => Item, From => Pool);
                        end;
                  or
                     accept Get_Current_Size (Size : out Natural_Count) do
                        Size := Length(Pool);
                     end;
                  or
                     when Empty(Pool) =>
                        accept Kill;
                        exit;
                  or
                     when Empty(Pool) =>
                        terminate;
                  end select;
               end loop;
            end;
         else
            declare

               Pool       : array (1..Size) of Item_Type;
               In_Index,
               Out_Index  : Count range Pool'Range := Pool'First;
               Card       : Count range 0..Pool'Length := 0;

            begin
               loop
                  select
                     when Card < Size =>
                        accept Write (Item : in Item_Type) do
                           Pool(In_Index) := Item;
                        end;
                        if In_Index < Pool'Last then
                           In_Index := In_Index + 1;
                        else
                           In_Index := Pool'First;
                        end if;
                        Card := Card + 1;
                  or
                     when Card > 0 =>
                        accept Read (Item : out Item_Type) do
                           Item := Pool(Out_Index);
                        end;
                        if Out_Index < Pool'Last then
                           Out_Index := Out_Index + 1;
                        else
                           Out_Index := Pool'First;
                        end if;
                        Card := Card - 1;
                  or
                     accept Get_Current_Size (Size : out Natural_Count) do
                        Size := Card;
                     end;
                  or
                     when Card = 0 =>
                        accept Kill;
                        exit;
                  or
                     when Card = 0 =>
                        terminate;
                  end select;
               end loop;
            end;
         end if;
      end if;
   end Buffer;


   function Current_Size (Of_Buffer : in Buffer) return Natural_Count is

      Size : Natural_Count;

   begin
      Of_Buffer.Get_Current_Size(Size);
      return Size;
   end Current_Size;

end Buffers;
