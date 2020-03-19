-- Adapted for the Bound-T Execution Time Analysis tool.
--
-- $Revision: 1.3 $
-- Tidorum changes $Log: hash_g.adb,v $
-- Tidorum changes Revision 1.3  2007-05-07 11:25:07  niklas
-- Tidorum changes Corrected procedure Modify to use Table( Start ).First *after*
-- Tidorum changes possible modification of this value by Key_Exists. Before this
-- Tidorum changes correction the new data went into the cell that was the First
-- Tidorum changes cell before Key_Exists, which is some arbitrary cell with the
-- Tidorum changes same hash-value but usually a different Key.
-- Tidorum changes


with Ada.Text_IO; use Ada.Text_IO;
with Next_Prime;
with Ada.Unchecked_Deallocation;
with Hash_G_Opt;


package body Hash_G is

   Empty : constant Integer := -1;
   -- If Some_Table.Hash_Array( Some_Index ).Next := Empty then the
   -- cell is empty.

   Last : constant Integer := -2;
   -- If Some_Table.Hash_Array( Some_Index ).Next := Last then it is
   -- the last cell in this collision chain.

   procedure Unchecked_Free_Key is new
      Ada.Unchecked_Deallocation( Key_Type, Key_Access );

   procedure Unchecked_Free_Data is new
      Ada.Unchecked_Deallocation( Data_Type, Data_Access );


   procedure Free_Key (Item : in out Key_Access)
   is
   begin

      if Hash_G_Opt.Deallocate then

         Unchecked_Free_Key (Item);

      else

         Item := null;

      end if;

   end Free_Key;


   procedure Free_Data (Item : in out Data_Access)
   is
   begin

      if Hash_G_Opt.Deallocate then

         Unchecked_Free_Data (Item);

      else

         Item := null;

      end if;

   end Free_Data;


   procedure Initialize
         ( Object : in out Hash_Table_Type ) is
   -- Prevents Object.Table to be null after
   -- definition of Object.
   begin
      Object.Table := new Hash_Array( 1..0 );
      -- empty array
   end Initialize;


   procedure Finalize
         ( Object : in out Hash_Table_Type ) is
   -- Destroys all cells Object.Table(..).Key and
   -- Object.Table(..).Date before demolition of
   -- Object.
   begin
      for U in Object.Table'Range loop
         Free_Key( Object.Table( U ).Key );
         Free_Data( Object.Table( U ).Data );
      end loop;
   end Finalize;


   procedure Adjust
         ( Object : in out Hash_Table_Type ) is
   -- After an assignment A := B (both Hash_Table_Type), the two
   -- records contain exactly the same; so everything after B.Table
   -- is shared between A and B. That's why it is necessary to copy
   -- all information after B.Table. After this operations, both A
   -- and B will have all the information absolutely independent.
      New_Table : Hash_Array_Access;
   begin
      New_Table := new Hash_Array( Object.Table'Range );
      for R in Object.Table'Range loop
         if Object.Table( R ).Key /= null then
            New_Table( R ).Key := new Key_Type'( Object.Table( R ).Key.all );
         end if;
         if Object.Table( R ).Data /= null then
            New_Table( R ).Data := new Data_Type'( Object.Table( R ).Data.all );
         end if;
         New_Table( R ).First := Object.Table( R ).First;
         New_Table( R ).Next := Object.Table( R ).Next;
      end loop;
      Object.Table := New_Table;
   end Adjust;


   function Create
         ( Minimum_Size : Positive )
      return Hash_Table_Type is

      Size : Positive := Next_Prime( Minimum_Size );
      Table_Access : Hash_Array_Access := new Hash_Array( 0 .. Size - 1 );

   begin
      for Index in Table_Access'Range loop
         Table_Access( Index ) := ( Key          => null,
                                    Data         => null,
                                    First        => Last,
                                    Next         => Empty );
      end loop;
      return ( Ada.Finalization.Controlled with
               Table => Table_Access, Counter => 0 );
   end Create;


   procedure Clear
         ( Hash_Table : in out Hash_Table_Type ) is

      Table : Hash_Array renames Hash_Table.Table.all;

   begin
      for Index in Table'Range loop
         if Table( Index ).Next /= Empty then
            Free_Key( Table( Index ).Key );
            Free_Data( Table( Index ).Data );
            Table( Index ).Next := Empty;
         end if;
         Table( Index ).First := Last;
      end loop;
      Hash_Table.Counter := 0;
   end Clear;


   function Hash_2
         ( Number      : in    Natural;
           Hash_Table  : in    Hash_Table_Type )
      return Positive is
   -- Must return a value between 1 and ( Hash_Table.Table'LENGTH-1 )
   begin
      if ( Number = 0) then
         return 1;
      else
         return ( Hash_Table.Table'Length - Number );
      end if;
   end Hash_2;


   procedure Insert
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type ) is

      Table : Hash_Array renames Hash_Table.Table.all;

      Start : constant Natural := Hash( Key ) mod Table'Length;
      Step  : constant Natural := Hash_2( Start, Hash_Table );

      Index    : Natural := Start;

   begin
      if Is_Full( Hash_Table ) then
         raise Table_Full_Error;
      elsif Key_Exists( Hash_Table, Key ) then
         raise Key_Exists_Error;
      end if;

      while Table( Index ).Next /= Empty loop
         Index := ( Index + Step ) mod Table'Length;
      end loop;
      -- Index now references the first empty cell

      -- Insert at the front of the collision chain
      Table( Index ).Next := Table( Start ).First;
      Table( Start ).First := Index;

      Table( Index ).Key  := new Key_Type'( Key );
      Table( Index ).Data := new Data_Type'( Data );
      Hash_Table.Counter := Hash_Table.Counter + 1;
   end Insert;


   procedure Insert
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type;
           Duplicate  :    out Boolean;
           Full       :    out Boolean ) is

   begin
      Duplicate := False;
      Full := False;
      Insert( Hash_Table, Key, Data );
   exception
      when Key_Exists_Error =>
         Duplicate := True;
      when Table_Full_Error =>
         Full := True;
      when others => raise;
   end Insert;


   procedure Remove
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type ) is


      Table : Hash_Array renames Hash_Table.Table.all;
      Start : constant Natural := Hash( Key ) mod Table'Length;
      Index : Integer;

   begin
      if Key_Exists( Hash_Table, Key ) then
         -- Key_Exists moves the cell to the front of the collision chain.
         Index := Table( Start ).First;
         Free_Key( Table( Index ).Key );
         Free_Data( Table( Index ).Data );
         Table( Start ).First := Table( Index ).Next;
         Table( Index ).Next := Empty;
         Hash_Table.Counter := Hash_Table.Counter - 1;
      else
         raise Key_Not_Found_Error;
      end if;
   end Remove;


   procedure Remove
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Found      :    out Boolean ) is

   begin
      Remove( Hash_Table, Key );
      Found := True;
   exception
      when Key_Not_Found_Error =>
         Found := False;
      when others => raise;
   end Remove;


   procedure Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           New_Data   : in     Data_Type ) is

      Table : Hash_Array renames Hash_Table.Table.all;

      Start : Natural := Hash( Key ) mod Table'Length;
      -- The start of the hash chain.

      First : Integer;
      -- The index of the existing cell for this Key, if there is one.

      New_Data_Store : Data_Access;

   begin

      if Key_Exists( Hash_Table, Key ) then
         -- Key_Exists moves the cell to the front of the chain by
         -- possibly changing Table( Start ).First and some Next links.

         First := Table( Start ).First;
         -- The index of the existing element for this Key.

         New_Data_Store := new Data_Type'( New_Data );
         -- The new data in a container.

         Free_Data( Table( First ).Data );
         -- Throwing away the existing data for this Key.

         Table( First ).Data := New_Data_Store;
         -- And replacing it with the new data.

      else

         raise Key_Not_Found_Error;

      end if;

   end Modify;


   procedure Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           New_Data   : in     Data_Type;
           Found      :    out Boolean ) is

   begin
      Modify( Hash_Table, Key, New_Data );
      Found := True;
   exception
      when Key_Not_Found_Error =>
         Found := False;
      when others => raise;
   end Modify;



   procedure Insert_Or_Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type;
           Newer      :    out Boolean ) is

   begin
      if Key_Exists( Hash_Table, Key ) then
         Modify( Hash_Table, Key, Data );
         Newer := False;
      else
         Insert( Hash_Table, Key, Data );
         Newer := True;
      end if;
   exception
      when others => raise;
   end Insert_Or_Modify;



   procedure Insert_Or_Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type;
           Newer      :    out Boolean;
           Full       :    out Boolean ) is

   begin
      Insert_Or_Modify( Hash_Table, Key, Data, Newer );
      Full := False;
   exception
      when Table_Full_Error =>
         Full := True;
      when others => raise;
   end Insert_Or_Modify;



   function Retrieve
         ( Hash_Table : in     Hash_Table_Type;
           Key        : in     Key_Type )
      return Data_Type is

      Table : Hash_Array renames Hash_Table.Table.all;

      Start : Natural := Hash( Key ) mod Table'Length;
      Index : Integer := Table( Start ).First;

   begin
      while Index /= Last loop
         if Table( Index ).Key.all = Key then
            return Table( Index ).Data.all;
         else
            Index := Table( Index ).Next;
         end if;
      end loop;
      raise Key_Not_Found_Error;
   end Retrieve;



   function Retrieve
         ( Hash_Table : in     Hash_Table_Type;
           Key        : in     Key_Type )
      return Data_Access is

      Table : Hash_Array renames Hash_Table.Table.all;

      Start : Natural := Hash( Key ) mod Table'Length;
      Index : Integer := Table( Start ).First;

   begin
      while Index /= Last loop
         if Table( Index ).Key.all = Key then
            return Table( Index ).Data;
         else
            Index := Table( Index ).Next;
         end if;
      end loop;
      raise Key_Not_Found_Error;
   end Retrieve;



   function Size
         ( Hash_Table : in Hash_Table_Type )
      return Natural is

   begin
      return Hash_Table.Table'Length;
   end Size;



   function Element_Count
         ( Hash_Table : in Hash_Table_Type)
      return Natural is

   begin
      return Hash_Table.Counter;
   end Element_Count;



   function Is_Full
         ( Hash_Table : in Hash_Table_Type )
      return Boolean is

   begin
      return Hash_Table.Counter = Hash_Table.Table'Length;
   end Is_Full;


   function Key_Exists
         ( Hash_Table : in     Hash_Table_Type;
           Key        : in     Key_Type )
      return Boolean is

      Table : Hash_Array renames Hash_Table.Table.all;

      Start    : Natural := Hash( Key ) mod Table'Length;
      Index    : Integer := Table( Start ).First;
      Previous : Integer := Index;

   begin
      while Index /= Last loop
         if Table( Index ).Key.all = Key then
            if Previous /= Index then ----not first element in chain
               -- Remove from the collision chain
               Table( Previous ).Next := Table( Index ).Next;
               -- Insert at the front of the collision chain
               Table( Index ).Next := Table( Start ).First;
               Table( Start ).First := Index;
            end if;
            return True;
         else
            Previous := Index;
            Index := Table( Index ).Next;
         end if;
      end loop;
      return False;
   end Key_Exists;



   function "="
         ( Left, Right : in     Hash_Table_Type )
      return Boolean is

      Set_Not_Found_Error : exception;

      procedure My_Action ( Key      : in     Key_Type;
                            Data     : in out Data_Type;
                            Continue : in out Boolean ) is
      Data_Right    : Data_Access;
      begin
         Data_Right := Retrieve ( Right, Key );
         if ( Data_Right.all /= Data ) then
            raise Set_Not_Found_Error;
         end if;
      end My_Action;

      procedure My_Visit is new Visit( My_Action );

      My_Left : Hash_Table_Type := Create( Left.Table'Length );

   begin
      if (( Left.Table'Length /= Right.Table'Length ) or
          ( Left.Counter /= Right.Counter )) then
        return False;
      else
         My_Visit ( Left );
      end if;
      return True;
   exception
      when Key_Not_Found_Error |
        Set_Not_Found_Error => return False;
      when others => raise;
   end "=";



   procedure Visit
         ( Hash_Table : in     Hash_Table_Type ) is

      Table : Hash_Array renames Hash_Table.Table.all;

      Continue : Boolean := True;

   begin
      for K in Table'Range loop
         if not Continue then
            exit;
         end if;
         if ( Table( K ).Key /= null ) then
            Action( Table( K ).Key.all,
                 Table( K ).Data.all,
                 Continue );
        end if;
     end loop;
   end Visit;


end Hash_G;
