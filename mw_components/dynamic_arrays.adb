-- GENERIC PACKAGE FOR DYNAMIC ARRAYS
   ----------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Thu May 28 18:12:05 1998
-- Update Count    : 2

-- Revision : 27-MAY-1986 by Mats Weber, use of derived subprograms.
-- Creation : 10-FEB-1986 by Mats Weber


package body Dynamic_Arrays is
---------------------------

   procedure Assign (Object : in out Dynamic_Array;
                     Index  : in Index_Type;
                     Value  : in Component_Type) is
   begin
      Replace(Key => Index, New_Item => Value, Within => Object);
   exception
      when I_C_Tables.Nonexistent_Key =>
         Insert(Key => Index, Item => Value, Into => Object);
   end Assign;


   function Component (From : Dynamic_Array; Index : Index_Type) return Component_Type is
   begin
      return Search(Key => Index, Within => From);
   exception
      when I_C_Tables.Nonexistent_Key =>
         raise Nonexistent_Index;
   end Component;


   function Present (Within : Dynamic_Array; Index : Index_Type) return Boolean is
   begin
      return Member(Key => Index, Of_Table => Within);
   end Present;


   function First (Of_Array : Dynamic_Array) return Index_Type is
   begin
      return Min(Of_Array);
   exception
      when I_C_Tables.Table_Empty =>
         raise Dynamic_Array_Empty;
   end First;


   function Last  (Of_Array : Dynamic_Array) return Index_Type is
   begin
      return Max(Of_Array);
   exception
      when I_C_Tables.Table_Empty =>
         raise Dynamic_Array_Empty;
   end Last;


   procedure Remove (From : in out Dynamic_Array; Index : in Index_Type) is
   begin
      I_C_Tables.Remove(Key => Index, From => I_C_Tables.Table(From));
   exception
      when I_C_Tables.Nonexistent_Key =>
         null;
   end Remove;

   procedure Remove (From      : in out Dynamic_Array;
                     Index     : in Index_Type;
                     Component : out Component_Type) is
   begin
      I_C_Tables.Remove(Key => Index, From => I_C_Tables.Table(From), Removed_Item => Component);
   exception
      when I_C_Tables.Nonexistent_Key =>
         raise Nonexistent_Index;
   end Remove;


   procedure Traversal (On_Array : in Dynamic_Array) is

      procedure Traverse is new I_C_Tables.Traversal(Action);

   begin
      Traverse(I_C_Tables.Table(On_Array));
   end Traversal;


   procedure Update_All (Within : in out Dynamic_Array) is

      procedure Do_Update_All is new I_C_Tables.Update_All(Update);

   begin
      Do_Update_All(I_C_Tables.Table(Within));
   end Update_All;


   procedure Assign (Object : in out Dynamic_Array; Value : in Dynamic_Array) is
   begin
      I_C_Tables.Assign(I_C_Tables.Table(Object), I_C_Tables.Table(Value));
   end Assign;


   function Card (Of_Array : Dynamic_Array) return Natural_Count is
   begin
      return I_C_Tables.Card(I_C_Tables.Table(Of_Array));
   end Card;


   function Empty (The_Array : Dynamic_Array) return Boolean is
   begin
      return I_C_Tables.Empty(I_C_Tables.Table(The_Array));
   end Empty;


   procedure Destroy (The_Array : in out Dynamic_Array) is
   begin
      I_C_Tables.Destroy(I_C_Tables.Table(The_Array));
   end Destroy;


   procedure Swap (Left, Right : in out Dynamic_Array) is
   begin
      I_C_Tables.Swap(I_C_Tables.Table(Left), I_C_Tables.Table(Right));
   end Swap;

end Dynamic_Arrays;
