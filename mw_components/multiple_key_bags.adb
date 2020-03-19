-- GENERIC BAGS WITH MULTIPLE KEYS
   -------------------------------

-- Creation : 23-DEC-1988 by Mats Weber.


with Lists,
     Exchange,
     Unchecked_Deallocation;

package body Multiple_Key_Bags is
------------------------------

   use Bag_Keys,
       Bag_Tables,
       Bag_Names;


   type Key_Number_Or_Zero is new Natural range 0..Number_Of_Keys;
   subtype Key_Number is Key_Number_Or_Zero range 1..Key_Number_Or_Zero'Last;
   type Bag_Operation is (Insert, Remove, Assign, Destroy, Finalize);

   The_Next_Key_Number : Key_Number_Or_Zero := 0;

   function Next_Key_Number return Key_Number is
   begin
      The_Next_Key_Number := The_Next_Key_Number + 1;
      return The_Next_Key_Number;
   exception
      when Constraint_Error =>
         raise Invalid_Instantiation;
   end Next_Key_Number;

   pragma Inline(Next_Key_Number);


   task Controller is
   ---------------

      entry Insert (Bag_Name   : in Bag_Names.Key;
                    Item       : in Item_Type;
                    Identifier : in Bag_Keys.Key);

      entry Remove (Bag_Name   : in Bag_Names.Key;
                    Item       : in Item_Type;
                    Identifier : in Bag_Keys.Key);

      entry Destroy (Bag_Name : in Bag_Names.Key);

      entry Assign (Object_Name, Value_Name : in Bag_Names.Key);

      entry Finalize;


      entry Get_Operation(Key_Number) (Operation : out Bag_Operation);

      entry Get_Item_To_Insert(Key_Number) (Bag_Name   : out Bag_Names.Key;
                                            Item       : out Item_Type;
                                            Identifier : out Bag_Keys.Key);

      entry Get_Item_To_Remove(Key_Number) (Bag_Name   : out Bag_Names.Key;
                                            Item       : out Item_Type;
                                            Identifier : out Bag_Keys.Key);

      entry Get_Bag_To_Destroy(Key_Number) (Bag_Name : out Bag_Names.Key);

      entry Get_Bags_To_Assign(Key_Number) (Object_Name, Value_Name : out Bag_Names.Key);

      entry Notify_End_Of_Operation(Key_Number);

   end Controller;


   procedure Insert (Item : in Item_Type; Into : in out Bag) is

      New_Id : constant Bag_Keys.Key := New_Key;

   begin
      Insert(Key  => New_Id,
             Item => Item,
             Into => Into.Contents);
      Controller.Insert(Bag_Name   => Into.Name,
                        Item       => Item,
                        Identifier => New_Id);
   end Insert;


   package body Access_By_Key is
   --------------------------

      package Bag_Key_Lists is new Lists(Item_Type => Bag_Keys.Key,
                                         Count     => Count);

      type Bag_Key_List_Access is access Bag_Key_Lists.List;

      package Key_To_Bag_Key_List_Tables is new Tables(Key_Type  => Key_Type,
                                                       Item_Type => Bag_Key_List_Access,
                                                       Count     => Count);

      type Key_To_Bag_Key_List_Table_Access is access Key_To_Bag_Key_List_Tables.Table;

      package Bag_Name_Tables is new Tables(Key_Type  => Bag_Names.Key,
                                            Item_Type => Key_To_Bag_Key_List_Table_Access,
                                            Count     => Natural);

      procedure Dispose is new Unchecked_Deallocation(Bag_Key_Lists.List,
                                                      Bag_Key_List_Access);

      procedure Dispose is new Unchecked_Deallocation(Key_To_Bag_Key_List_Tables.Table,
                                                      Key_To_Bag_Key_List_Table_Access);

      use Bag_Key_Lists,
          Key_To_Bag_Key_List_Tables,
          Bag_Name_Tables;

      All_Bags  : Bag_Name_Tables.Table;
      This_Key  : constant Key_Number := Next_Key_Number;

      task Key_Manager;
      ----------------


      procedure Remove (Key : in Key_Type; From : in out Bag) is

         Trash : Item_Type;

      begin
         Remove(Key, From, Removed_Item => Trash);
      end Remove;

      procedure Remove (Key : in Key_Type; From : in out Bag; Removed_Item : out Item_Type) is
      begin
         declare

            Removed_Id        : constant Bag_Keys.Key :=
                                Element(Head(Of_List => Search(Key    => Key,
                                                               Within => Search(Key    => From.Name,
                                                                                Within => All_Bags).all).all));

            Item_Just_Removed : Item_Type;

         begin
            Remove(Key          => Removed_Id,
                   From         => From.Contents,
                   Removed_Item => Item_Just_Removed);
            Controller.Remove(Bag_Name   => From.Name,
                              Item       => Item_Just_Removed,
                              Identifier => Removed_Id);
            Removed_Item := Item_Just_Removed;
         end;
      exception
         when Bag_Name_Tables.Nonexistent_Key | Key_To_Bag_Key_List_Tables.Nonexistent_Key =>
            raise Nonexistent_Key;
      end Remove;


      function Search (Key : Key_Type; Within : Bag) return Item_Type is
      begin
         return Search(Key    => Element(Head(Of_List => Search(Key    => Key,
                                                                Within => Search(Key    => Within.Name,
                                                                                 Within => All_Bags).all).all)),
                       Within => Within.Contents);
      exception
         when Bag_Name_Tables.Nonexistent_Key | Key_To_Bag_Key_List_Tables.Nonexistent_Key =>
            raise Nonexistent_Key;
      end Search;

      function Search (Key : Key_Type; Within : Bag) return List is
      begin
         declare

            Item_Id_List : Bag_Key_Lists.List renames Search(Key    => Key,
                                                             Within => Search(Key    => Within.Name,
                                                                              Within => All_Bags).all).all;
            Result       : List(1..Length(Item_Id_List));
            Index        : Count range 0..Result'Last := 0;

            procedure Put_Into_Result (Identifier : in Bag_Keys.Key) is
            begin
               Index := Index + 1;
               Result(Index) := Search(Key => Identifier, Within => Within.Contents);
            end Put_Into_Result;

            pragma Inline(Put_Into_Result);

            procedure Fill_Result is new Bag_Key_Lists.Traversal(Put_Into_Result);

         begin
            Fill_Result(Item_Id_List);
            return Result;
         end;
      exception
         when Bag_Name_Tables.Nonexistent_Key | Key_To_Bag_Key_List_Tables.Nonexistent_Key =>
            declare

               Result : List(1..0);

            begin
               return Result;
            end;
      end Search;


      function Member (Key : Key_Type; Of_Bag : Bag) return Boolean is
      begin
         return Member(Key      => Key,
                       Of_Table => Search(Key    => Of_Bag.Name,
                                          Within => All_Bags).all);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            return False;
      end Member;


      function Min (Of_Bag : Bag) return Key_Type is
      begin
         return Min(Of_Table => Search(Key    => Of_Bag.Name,
                                       Within => All_Bags).all);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            raise Bag_Empty;
      end Min;

      function Max (Of_Bag : Bag) return Key_Type is
      begin
         return Max(Of_Table => Search(Key    => Of_Bag.Name,
                                       Within => All_Bags).all);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            raise Bag_Empty;
      end Max;


      function Min (Of_Bag : Bag) return Item_Type is
      begin
         return Search(Key    => Element(Head(Min(Of_Table => Search(Key    => Of_Bag.Name,
                                                                     Within => All_Bags).all).all)),
                       Within => Of_Bag.Contents);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            raise Bag_Empty;
      end Min;

      function Max (Of_Bag : Bag) return Item_Type is
      begin
         return Search(Key    => Element(Tail(Max(Of_Table => Search(Key    => Of_Bag.Name,
                                                                     Within => All_Bags).all).all)),
                       Within => Of_Bag.Contents);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            raise Bag_Empty;
      end Max;

      procedure Remove_Min (From : in out Bag) is

         Trash : Item_Type;

      begin
         Remove_Min(From => From, Min => Trash);
      end Remove_Min;

      procedure Remove_Min (From : in out Bag; Min : out Item_Type) is
      begin
         declare

            Removed_Id        : constant Bag_Keys.Key :=
                                Element(Head(Of_List => Key_To_Bag_Key_List_Tables.Min
                                                           (Of_Table => Search(Key    => From.Name,
                                                                               Within => All_Bags).all).all));

            Item_Just_Removed : Item_Type;

         begin
            Remove(Key          => Removed_Id,
                   From         => From.Contents,
                   Removed_Item => Item_Just_Removed);
            Controller.Remove(Bag_Name   => From.Name,
                              Item       => Item_Just_Removed,
                              Identifier => Removed_Id);
            Min := Item_Just_Removed;
         end;
      exception
         when Bag_Name_Tables.Nonexistent_Key | Key_To_Bag_Key_List_Tables.Table_Empty =>
            raise Bag_Empty;
      end Remove_Min;


      procedure Remove_Max (From : in out Bag) is

         Trash : Item_Type;

      begin
         Remove_Max(From => From, Max => Trash);
      end Remove_Max;

      procedure Remove_Max (From : in out Bag; Max : out Item_Type) is
      begin
         declare

            Removed_Id        : constant Bag_Keys.Key :=
                                Element(Tail(Of_List => Key_To_Bag_Key_List_Tables.Max
                                                           (Of_Table => Search(Key    => From.Name,
                                                                               Within => All_Bags).all).all));

            Item_Just_Removed : Item_Type;

         begin
            Remove(Key          => Removed_Id,
                   From         => From.Contents,
                   Removed_Item => Item_Just_Removed);
            Controller.Remove(Bag_Name   => From.Name,
                              Item       => Item_Just_Removed,
                              Identifier => Removed_Id);
            Max := Item_Just_Removed;
         end;
      exception
         when Bag_Name_Tables.Nonexistent_Key | Key_To_Bag_Key_List_Tables.Table_Empty =>
            raise Bag_Empty;
      end Remove_Max;


      procedure Traversal (On_Bag : in Bag) is

         procedure Traverse_List (Key : in Key_Type; The_List : in Bag_Key_List_Access) is

            procedure Action (Identifier : in Bag_Keys.Key) is
            begin
               Action(Item => Search(Key    => Identifier,
                                     Within => On_Bag.Contents));
            end Action;

            pragma Inline(Action);

            procedure Traverse_List is new Bag_Key_Lists.Traversal(Action);

         begin
            Traverse_List(The_List.all);
         end Traverse_List;

         procedure Traverse_All_Lists is new Key_To_Bag_Key_List_Tables.Traversal(Traverse_List);

      begin
         Traverse_All_Lists(Search(Key    => On_Bag.Name,
                                   Within => All_Bags).all);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            null;
      end Traversal;


      procedure Update_All (Within : in out Bag) is

         procedure Update_List (Key : in Key_Type; The_List : in Bag_Key_List_Access) is

            procedure Update_Item (Identifier : in Bag_Keys.Key) is

               procedure Modify is new Bag_Tables.Update(Update_Item);

            begin
               Modify(Key    => Identifier,
                      Within => Within.Contents);
            end Update_Item;

            pragma Inline(Update_Item);

            procedure Update_List is new Bag_Key_Lists.Traversal(Update_Item);

         begin
            Update_List(The_List.all);
         end Update_List;

         procedure Update_All_Lists is new Key_To_Bag_Key_List_Tables.Traversal(Update_List);

      begin
         Update_All_Lists(Search(Key    => Within.Name,
                                 Within => All_Bags).all);
      exception
         when Bag_Name_Tables.Nonexistent_Key =>
            null;
      end Update_All;


      task body Key_Manager is
      ---------------------

         Operation : Bag_Operation;

      begin
         loop
            Controller.Get_Operation(This_Key) (Operation);
            case Operation is
               when Insert =>
                  declare

                     Bag_Name    : Bag_Names.Key;
                     The_Bag     : Key_To_Bag_Key_List_Table_Access;
                     Item        : Item_Type;
                     Identifier  : Bag_Keys.Key;

                  begin
                     Controller.Get_Item_To_Insert(This_Key) (Bag_Name, Item, Identifier);
                     begin
                        The_Bag := Search(Key    => Bag_Name,
                                          Within => All_Bags);
                     exception
                        when Bag_Name_Tables.Nonexistent_Key =>
                           The_Bag := new Key_To_Bag_Key_List_Tables.Table;
                           Insert(Key  => Bag_Name,
                                  Item => The_Bag,
                                  Into => All_Bags);
                     end;
                     begin
                        Insert_At_Tail(Item => Identifier,
                                       Into => Search(Key    => Key_Of(Item),
                                                      Within => The_Bag.all).all);
                     exception
                        when Key_To_Bag_Key_List_Tables.Nonexistent_Key =>
                           declare

                              New_Bag_Key_List : constant Bag_Key_List_Access := new Bag_Key_Lists.List;

                           begin
                              Insert_At_Tail(Item => Identifier, Into => New_Bag_Key_List.all);
                              Insert(Key  => Key_Of(Item),
                                     Item => New_Bag_Key_List,
                                     Into => The_Bag.all);
                           end;
                     end;
                  end;
               when Remove =>
                  declare

                     Bag_Name    : Bag_Names.Key;
                     The_Bag     : Key_To_Bag_Key_List_Table_Access;
                     Item        : Item_Type;
                     Identifier  : Bag_Keys.Key;

                  begin
                     Controller.Get_Item_To_Remove(This_Key) (Bag_Name, Item, Identifier);
                     The_Bag := Search(Key    => Bag_Name,
                                       Within => All_Bags);
                     declare

                        The_Bag_Key_List : Bag_Key_List_Access := Search(Key    => Key_Of(Item),
                                                                         Within => The_Bag.all);

                        P                : Bag_Key_Lists.Pointer := Head(The_Bag_Key_List.all);

                     begin
                        while Element(P) /= Identifier loop
                           P := Next(P);
                        end loop;
                        Remove(Position => P, From => The_Bag_Key_List.all);
                        if Empty(The_Bag_Key_List.all) then
                           Remove(Key => Key_Of(Item), From => The_Bag.all);
                           Dispose(The_Bag_Key_List);
                        end if;
                     end;
                     if Empty(The_Bag.all) then
                        Remove(Key => Bag_Name, From => All_Bags);
                        Dispose(The_Bag);
                     end if;
                  end;
               when Assign =>
                  declare

                     Object_Name,
                     Value_Name      : Bag_Names.Key;
                     Object_Bag      : Key_To_Bag_Key_List_Table_Access;

                  begin
                     Controller.Get_Bags_To_Assign(This_Key) (Object_Name, Value_Name);
                     begin
                        Object_Bag := Search(Key    => Object_Name,
                                             Within => All_Bags);
                     exception
                        when Bag_Name_Tables.Nonexistent_Key =>
                           Object_Bag := new Key_To_Bag_Key_List_Tables.Table;
                           Insert(Key  => Object_Name,
                                  Item => Object_Bag,
                                  Into => All_Bags);
                     end;
                     declare

                        procedure Assign (Object : in out Key_To_Bag_Key_List_Tables.Table;
                                          Value  : in Key_To_Bag_Key_List_Tables.Table) is

                           procedure Copy_Into_Object (Key   : in Key_Type;
                                                       Items : in Bag_Key_List_Access) is

                              Copy_Of_Items : constant Bag_Key_List_Access := new Bag_Key_Lists.List;

                           begin
                              Assign(Object => Copy_Of_Items.all, Value => Items.all);
                              Insert(Key  => Key,
                                     Item => Copy_Of_Items,
                                     Into => Object);
                           end Copy_Into_Object;

                           pragma Inline(Copy_Into_Object);

                           procedure Traverse_And_Copy_Into_Object is
                              new Key_To_Bag_Key_List_Tables.Traversal(Copy_Into_Object);

                        begin
                           Destroy(Object);
                           Traverse_And_Copy_Into_Object(Value);
                        end Assign;

                     begin
                        Assign(Object => Object_Bag.all,
                               Value  => Search(Key    => Value_Name,
                                                Within => All_Bags).all);
                     exception
                        when Bag_Name_Tables.Nonexistent_Key =>
                           -- VALUE is empty
                           null;
                     end;
                  end;
               when Destroy =>
                  declare

                     Bag_Name    : Bag_Names.Key;
                     The_Bag     : Key_To_Bag_Key_List_Table_Access;

                     procedure Destroy (Key   : in Key_Type;
                                        Items : in out Bag_Key_List_Access) is
                     begin
                        Destroy(Items.all);
                        Dispose(Items);
                     end Destroy;

                     pragma Inline(Destroy);

                     procedure Destroy_Contents is new Key_To_Bag_Key_List_Tables.Update_All(Destroy);

                  begin
                     Controller.Get_Bag_To_Destroy(This_Key) (Bag_Name);
                     Remove(Key          => Bag_Name,
                            From         => All_Bags,
                            Removed_Item => The_Bag);
                     Destroy_Contents(The_Bag.all);
                     Destroy(The_Bag.all);
                  exception
                     when Bag_Name_Tables.Nonexistent_Key =>
                        null;
                  end;
               when Finalize =>
                  exit;
            end case;
            Controller.Notify_End_Of_Operation(This_Key);
         end loop;
      end Key_Manager;

   end Access_By_Key;


   function Card (Of_Bag : Bag) return Natural_Count is
   begin
      return Card(Of_Table => Of_Bag.Contents);
   end Card;


   function Empty (The_Bag : Bag) return Boolean is
   begin
      return Empty(The_Table => The_Bag.Contents);
   end Empty;


   procedure Assign (Object : in out Bag; Value : in Bag) is
   begin
      Assign(Object => Object.Contents, Value => Value.Contents);
      Controller.Assign(Object_Name => Object.Name, Value_Name => Value.Name);
   end Assign;


   procedure Destroy (The_Bag : in out Bag) is
   begin
      Destroy(The_Bag.Contents);
      Controller.Destroy(Bag_Name => The_Bag.Name);
   end Destroy;


   procedure Swap (Left, Right : in out Bag) is

      procedure Swap is new Exchange(Bag_Names.Key);

   begin
      Swap(Left.Contents, Right.Contents);
      Swap(Left.Name,     Right.Name);
   end Swap;


   procedure Check_Instantiation is
   begin
      if The_Next_Key_Number /= Key_Number'Last then
         raise Invalid_Instantiation;
      end if;
   end Check_Instantiation;

   pragma Inline(Check_Instantiation);


   procedure Finalize is
   begin
      Controller.Finalize;
   end Finalize;


   task body Controller is
   --------------------
   begin
      loop
         select
            accept Insert (Bag_Name   : in Bag_Names.Key;
                           Item       : in Item_Type;
                           Identifier : in Bag_Keys.Key)
            do
               Check_Instantiation;
               for The_Key in Key_Number loop
                  accept Get_Operation(The_Key) (Operation : out Bag_Operation) do
                     Operation := Insert;
                  end;
                  accept Get_Item_To_Insert(The_Key) (Bag_Name   : out Bag_Names.Key;
                                                      Item       : out Item_Type;
                                                      Identifier : out Bag_Keys.Key)
                  do
                     Bag_Name   := Insert.Bag_Name;
                     Item       := Insert.Item;
                     Identifier := Insert.Identifier;
                  end Get_Item_To_Insert;
               end loop;
               for The_Key in Key_Number loop
                  accept Notify_End_Of_Operation(The_Key);
               end loop;
            end Insert;
         or
            accept Remove (Bag_Name   : in Bag_Names.Key;
                           Item       : in Item_Type;
                           Identifier : in Bag_Keys.Key)
            do
               Check_Instantiation;
               for The_Key in Key_Number loop
                  accept Get_Operation(The_Key) (Operation : out Bag_Operation) do
                     Operation := Remove;
                  end;
                  accept Get_Item_To_Remove(The_Key) (Bag_Name   : out Bag_Names.Key;
                                                      Item       : out Item_Type;
                                                      Identifier : out Bag_Keys.Key)
                  do
                     Bag_Name   := Remove.Bag_Name;
                     Item       := Remove.Item;
                     Identifier := Remove.Identifier;
                  end Get_Item_To_Remove;
               end loop;
               for The_Key in Key_Number loop
                  accept Notify_End_Of_Operation(The_Key);
               end loop;
            end Remove;
         or
            accept Assign (Object_Name, Value_Name : in Bag_Names.Key) do
               Check_Instantiation;
               for The_Key in Key_Number loop
                  accept Get_Operation(The_Key) (Operation : out Bag_Operation) do
                     Operation := Assign;
                  end;
                  accept Get_Bags_To_Assign(The_Key) (Object_Name, Value_Name : out Bag_Names.Key) do
                     Value_Name  := Assign.Value_Name;
                     Object_Name := Assign.Object_Name;
                  end Get_Bags_To_Assign;
               end loop;
               for The_Key in Key_Number loop
                  accept Notify_End_Of_Operation(The_Key);
               end loop;
            end Assign;
         or
            accept Destroy (Bag_Name : in Bag_Names.Key) do
               Check_Instantiation;
               for The_Key in Key_Number loop
                  accept Get_Operation(The_Key) (Operation : out Bag_Operation) do
                     Operation := Destroy;
                  end;
                  accept Get_Bag_To_Destroy(The_Key) (Bag_Name : out Bag_Names.Key) do
                     Bag_Name := Destroy.Bag_Name;
                  end;
               end loop;
               for The_Key in Key_Number loop
                  accept Notify_End_Of_Operation(The_Key);
               end loop;
            end Destroy;
         or
            accept Finalize do
               Check_Instantiation;
               for The_Key in Key_Number loop
                  accept Get_Operation(The_Key) (Operation : out Bag_Operation) do
                     Operation := Finalize;
                  end;
               end loop;
            end Finalize;
            exit;
         end select;
      end loop;
   end Controller;

end Multiple_Key_Bags;
