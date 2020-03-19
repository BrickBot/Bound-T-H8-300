-- Storage.Cell_Store (body)
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.10 $
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: storage-cell_store.adb,v $
-- Revision 1.10  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.9  2013-02-12 08:47:20  niklas
-- BT-CH-0245: Global volatile cells and "volatile" assertions.
--
-- Revision 1.8  2011-09-09 14:51:51  niklas
-- Added function Initial_Cell_Variables, for use in ALF export.
--
-- Revision 1.7  2009-11-27 11:28:08  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.6  2008/06/18 20:52:57  niklas
-- BT-CH-0130: Data pointers relative to initial-value cells.
--
-- Revision 1.5  2008/04/26 19:19:44  niklas
-- BT-CH-0124: Joint loop counters and induction variables.
--
-- Revision 1.4  2007/10/19 17:07:28  niklas
-- Added Fault outputs to New_Cell. In case.
--
-- Revision 1.3  2007/09/23 12:15:08  niklas
-- BT-CH-0078: Storage.Cell_Store uses Cell_Spec as primary key.
--
-- Revision 1.2  2007/01/25 21:25:19  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/04/25 09:33:06  niklas
-- First Tidorum version.
--


with Bags;               -- MW_Components
with Hash_G;             -- LGL components
with Hash_G.Changes_G;   -- LGL components
with Interfaces;
with Output;
with Processor;
with Storage.Opt;
with Storage.Reserved_Names;
with Storage.Volatiles;
with Unbounded_Vectors;


package body Storage.Cell_Store is


   --
   ---   Cell-storage data structures
   --
   --
   -- Cells are stored and are accessible in three ways:
   --
   -- > By the Cell_Spec, which is the primary way to access a cell.
   --   See Cell_Bags and Spec_Bag below.
   --
   -- > By the Index, which is useful for cell-sets represented as
   --   bit-maps indexed by the cell index. See Cell_Vectors and
   --   Index_Vector, below.
   --
   -- > By the Name, which is useful when assertions talk about cells.
   --   See Cell_Hash and Name_Hash, below.
   --
   -- In addition, there is a simple list of those cells for which
   -- initial-value cells are defined. This is expected to be a very
   -- small set, a handful at most.


   package Cell_Bags is new Bags (
      Processor.Cell_Spec_T,   -- Key_Type
      Cell_T,                  -- Item_Type
      Spec_Of,                 -- Key_Of
      Processor."=",           -- "=" (Key_Type)
      Processor."<",           -- "<" (Key_Type)
      Natural);                -- Count
   --
   -- The set of cells, indexed by the Cell_Spec. This is the primary
   -- container of cells; it ensures that there is at most one Cell_T
   -- for each value of Cell_Spec.


   package Cell_Vectors is new Unbounded_Vectors (
      Element_Type   => Cell_T,
      Vector_Type    => Cell_List_T,
      Initial_Size   => 20,
      Size_Increment => 20,
      Deallocate     => Opt.Deallocate);


   function Char_XOR (S : String) return Natural
   is
      use Interfaces;

      Hash : Unsigned_32 := S'Length;

   begin

      for I in S'range loop
         Hash :=     Rotate_Left (Hash, 8)
                 xor Unsigned_32 (Character'pos(S(I)));
      end loop;

      return Natural (Hash mod Unsigned_32(Natural'Last));

   end Char_XOR;


   package Cell_Hash is new Hash_G (
      Key_Type  => String,
      Data_Type => Cell_T,
      Hash      => Char_XOR);

   package Cell_Hash_Changes is new Cell_Hash.Changes_G;


   Initial_Size_C : constant := 1023;
   --
   -- Initial size of the hash table.


   Size_Increment_C : constant := 511;
   --
   -- Amount by which hash table is grown when it gets full.


   --
   ---   Cell-storage variables
   --


   Spec_Bag : Cell_Bags.Bag (Duplicate_Keys_Allowed => False);
   --
   -- The cells, keyed by Cell_Spec.


   Index_Vector : Cell_Vectors.Unbounded_Vector;
   --
   -- The cells, keyed by Index.


   Name_Hash : Cell_Hash.Hash_Table_Type :=
      Cell_Hash.Create (Minimum_Size => Initial_Size_C);
   --
   -- The cells, keyed by Name.
   -- It will be resized (grown) if necessary.


   Max_Init_Variables : constant := 20;
   --
   -- The maximum number of cells for which initial cells
   -- are defined.


   Init_Variables : Cell_List_T (1 .. Max_Init_Variables);
   Num_Init_Vars  : Natural := 0;
   --
   -- The cells for which initial-value cells are defined
   -- are Init_Variables(1 .. Num_Init_Vars).


   Volatile_Cell_Tally : Natural := 0;
   --
   -- The number of volatile cells created so far.


   function Full_Image (Cell : Cell_T) return String
   --
   -- A fuller image.
   --
   is
   begin

      return
           "#"
         & Cell_Index_T'Image (Cell.Index)
         & ", name """
         & Cell.Name.all
         & """, spec "
         & Processor.Image (Cell.Spec.all);

   end Full_Image;


   function New_Cell (Spec : Processor.Cell_Spec_T) return Cell_T
   --
   -- A new cell added to the store.
   -- Precondition: the Spec is a new one (not yet in Spec_Bag).
   --
   is

      Cell : Cell_T;
      -- The new cell.

      Group : Processor.Alias_Group_T;
      -- The alias group for a new cell.

      Width : Positive;
      -- The width of the cell, from its Spec.

      Old_Size, New_Size : Positive;
      -- For resizing Name_Hash.

      Dup_Cell : Cell_T;
      -- A possible duplicate cell, with the same name as the Cell.
      -- It should not exist...

   begin

      -- Create the new cell:

      Group := Processor.Alias_Group (Spec);

      Width := Processor.Width_Of (Spec);

      if Width > Positive (Arithmetic_Base.Width_T'Last) then

         Output.Fault (
            Location => "Storage.Cell_Store.New_Cell",
            Text     =>
                 "Width_Of ("
               & Processor.Image (Spec)
               & ") = "
               & Output.Image (Width));

         Width := Positive (Arithmetic_Base.Width_T'Last);

      end if;

      Cell := new Cell_Object_T'(
         Spec         => new Processor.Cell_Spec_T'(Spec),
         Name         => new String'(Processor.Image (Spec)),
         Index        => Cell_Index_T (Cell_Vectors.Next (Index_Vector)),
         Width        => Arithmetic_Base.Width_T (Width),
         Volatile     => Volatiles.In_Volatile_Range (Spec),
         Vol_Index    => Volatile_Cell_Index_T'Last,
         Counter      => Processor.Can_Count (Spec),
         Alias_Group  => Group,
         Alias_Range  => Processor.Alias_Range (Group),
         Is_Initial   => False,
         Initial_Cell => No_Cell);

      -- Count volatile cells:

      if Cell.Volatile then

         Volatile_Cell_Tally := Volatile_Cell_Tally + 1;

         Cell.Vol_Index := Volatile_Cell_Index_T (Volatile_Cell_Tally);

      end if;

      -- Check that the Name is not a reserved one:

      for N in Reserved_Names.Name'Range loop

         if Cell.Name.all = Reserved_Names.Name(N).all then

            Output.Fault (
               Location => "Storage.Cell_Store.New_Cell",
               Text     => "Reserved name " & Cell.Name.all);

         end if;

      end loop;

      -- Insert the new cell in the stores:

      Cell_Bags.Insert (Item => Cell, Into => Spec_Bag);

      Cell_Vectors.Append (To => Index_Vector, Value => Cell);

      if Cell_Hash.Is_Full (Name_Hash) then

         Old_Size := Cell_Hash.Size (Name_Hash);
         New_Size := Old_Size + Size_Increment_C;

         Output.Note (Text =>
            "Cell-hash-table was grown from"
            & Positive'Image (Old_Size)
            & " to (at least)"
            & Positive'Image (New_Size)
            & " elements.");

         Cell_Hash_Changes.Resize (
            Hash_Table   => Name_Hash,
            Minimum_Size => New_Size);

      end if;

      Cell_Hash.Insert (
         Hash_Table => Name_Hash,
         Key        => Cell.Name.all,
         Data       => Cell);

      if Cell.Volatile and Opt.Trace_Volatile then

         Output.Trace (
              "Volatile cell"
            & Output.Field_Separator
            & Name_Of (Cell));

      end if;

      -- Return the new cell:

      return Cell;

   exception

   when Cell_Hash.Key_Exists_Error =>

     Output.Fault (
        Location => "Storage.Cell_Store.New_Cell",
        Text     =>
             "Duplicate cell"
           & Output.Field_Separator
           & Full_Image (Cell));

     Dup_Cell := Cell_By_Name (Cell.Name.all);

     Output.Fault (
        Location => "Storage.Cell_Store.New_Cell",
        Text     =>
             "Previous  cell"
           & Output.Field_Separator
           & Full_Image (Dup_Cell));

      return Dup_Cell;

   end New_Cell;


   function Cell_By_Spec (
      Spec              : Processor.Cell_Spec_T;
      Initial_Value_For : Cell_T)
   return Cell_T
   is

      Cell : Cell_T;
      -- The result.

   begin

      Cell := Cell_Bags.Search (Key => Spec, Within => Spec_Bag);
      -- Succeeds if the cell is already in the store, otherwise
      -- propagates Nonexistent_Key.

      if Initial_Value_For /= No_Cell then

         Output.Fault (
            Location => "Storage.Cell_Store.Cell_By_Spec",
            Text     =>
                 "Initial-value cell "
               & Image (Cell)
               & " already exists.");

      end if;

      return Cell;

   exception

   when Cell_Bags.Nonexistent_Key =>
      -- A new cell.

      Cell := New_Cell (Spec);

      if Initial_Value_For /= No_Cell then
         -- This Cell should be an initial-value cell.

         Cell.Initial_Cell := Cell;

         if Initial_Value_For.Initial_Cell = No_Cell then
            -- Good, the given variable cell does not yet have
            -- an initial-value cell.

            Cell.Is_Initial := True;
            Initial_Value_For.Initial_Cell := Cell;

            if Num_Init_Vars < Init_Variables'Last then
               -- We can record this initial-cell/variable-cell pair.

               Num_Init_Vars := Num_Init_Vars + 1;

               Init_Variables(Num_Init_Vars) := Initial_Value_For;

            else

               Output.Fault (
                  Location => "Storage.Cell_Store.Cell_By_Spec",
                  Text     => "Overflow of the initial-cell list.");

            end if;

         else

            Output.Fault (
               Location => "Storage.Cell_Store.Cell_By_Spec",
               Text     =>
                    "Variable cell "
                  & Image (Initial_Value_For)
                  & " already has initial-value cell "
                  & Image (Initial_Value_For.Initial_Cell)
                  & ", not changed to "
                  & Image (Cell));

         end if;

      end if;

      return Cell;

   end Cell_By_Spec;


   function Cell_By_Name (Name : String) return Cell_T
   is
   begin

      return Cell_Hash.Retrieve (
         Hash_Table => Name_Hash,
         Key        => Name);

   exception

   when Cell_Hash.Key_Not_Found_Error =>

      raise No_Such_Cell;

   end Cell_By_Name;


   function Number_Of_Cells return Natural
   is
   begin

      return Cell_Vectors.Length (Index_Vector);

   end Number_Of_Cells;


   function Number_Of_Volatile_Cells return Natural
   is
   begin

      return Volatile_Cell_Tally;

   end Number_Of_Volatile_Cells;


   function Cell_At (Index : Cell_Index_T) return Cell_T
   is
   begin

      return Cell_Vectors.Element (Index_Vector, Positive (Index));

   end;


   function Initial_Cell_Variables
   return Cell_List_T
   is
   begin

      return Init_Variables(1 .. Num_Init_Vars);

   end Initial_Cell_Variables;


end Storage.Cell_Store;
