--:dbpool with GNAT.Debug_Pools;

with Ada.Finalization;

generic
   type Key_Type(<>) is private;
   -- This is the type of the keys.

   type Data_Type(<>) is private;
   -- The type of the data to store.

   with function Hash (Key: Key_Type) return Natural;
   -- The result modulo the size of the hash table is used as index.

   with function "="( Left, Right : Key_Type ) return Boolean is <>;
   -- Defines the equality of the keys.

   with function "="( Left, Right : Data_Type ) return Boolean is <>;
   -- Defines the equality of the data.
   -- vorlaeufig weggelassen !!

package Hash_G is

   type Hash_Table_Type is private;

   type Data_Access is access Data_Type;

   --:dbpool Data_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Data_Access'Storage_Pool use Data_Pool;

   function Create( Minimum_Size : Positive ) return Hash_Table_Type;
   -- The size of the hash table will be the next prime number
   -- bigger than 'Minimum_Size'



   -- Exceptions --
   -- ========== --

   Table_Full_Error         : exception;

   Key_Exists_Error         : exception;

   Key_Not_Found_Error      : exception;



   -- Constructors --
   -- ============ --

   procedure Clear
         ( Hash_Table : in out Hash_Table_Type );
   -- Empties the hash table of all its Elements.


   procedure Insert
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type );
   -- Inserts a new element, consisting of the 'Key' and the 'Data', in
   -- the hash table.  Collisions are treated by the 'open-addressing/
   -- double-hashing'-method.  If the hash table is full, then the
   -- exception 'Table_Full_Error' is raised. If the 'Key' is already
   -- contained in the table, then the exception
   -- 'Key_Exists_Error' is raised.


   procedure Insert
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type;
           Duplicate  :    out Boolean;
           Full       :    out Boolean );
   -- Inserts the couple (Key, Data) into the hash table. If the table
   -- is full, no action is taken, and Full is set to TRUE
   -- If the table is not full, and an entry with the given key is already
   -- in the table, then no action is taken, and Duplicate is set to TRUE


   procedure Remove
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type );
   -- Removes an element in the table.  If the 'Key' is not contained
   -- in the table, then the exception 'Key_Not_Found_Error' is raised.


   procedure Remove
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Found      :    out Boolean );
   -- Removes the element with key 'Key' in the table. If this key is
   -- not contained, nothing is changed except that 'Found' is set to FALSE.



   procedure Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           New_Data   : in     Data_Type );
   -- Modifies the data of the element with the key 'Key'. If this key
   -- is not contained in the hash table, the exception 'Key_Not_Found_Error'
   -- is raised.


   procedure Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           New_Data   : in     Data_Type;
           Found      :    out Boolean );
   -- Modifies the data of the element with key 'Key'. If this key
   -- is not contained in the hash table, the flag 'Found' is set to
   -- FALSE.


   procedure Insert_Or_Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type;
           Newer      :    out Boolean );
   -- Inserts the new element ('Key', 'Data') if it is not already
   -- contained resp. modifies the 'Data' of the contained 'Key'.
   -- The exception 'Table_Full_Error' is raised if necessary.


   procedure Insert_Or_Modify
         ( Hash_Table : in out Hash_Table_Type;
           Key        : in     Key_Type;
           Data       : in     Data_Type;
           Newer      :    out Boolean;
           Full       :    out Boolean );
   -- Inserts or modifies the element ('Key', 'Data').
   -- Same behavior as above, except that no exception is
   -- raised if the table is full, but the flag 'Full" is
   -- set to True.


   -- Selectors --
   -- ========= --

   function Retrieve
         ( Hash_Table : in     Hash_Table_Type;
           Key        : in     Key_Type )
      return Data_Type;
   function Retrieve
         ( Hash_Table : in     Hash_Table_Type;
           Key        : in     Key_Type )
      return Data_Access;
   -- Finds an element in the table.  If the 'Key' is not contained in
   -- the table, then the exception 'Key_Not_Found_Error' is raised.



   function Size
         ( Hash_Table : in     Hash_Table_Type )
      return Natural;
   -- Returns the usuable size of the table.


   function Element_Count
         ( Hash_Table : in     Hash_Table_Type)
      return Natural;
   -- Returns the number of elements stored in the table.


   function Is_Full
         ( Hash_Table : in     Hash_Table_Type )
      return Boolean;
   -- Returns TRUE if the table is already full, FALSE if not.


   function Key_Exists
         ( Hash_Table : in     Hash_Table_Type;
           Key        : in     Key_Type )
      return Boolean;
   -- Returns TRUE if the mentioned Key is already contained in the table.



   function "="
        ( Left, Right : in     Hash_Table_Type)
     return Boolean;
   -- Returns TRUE if the two hash tables are equal. Equality means
   -- that the tables have the same size and contain the same
   -- elements (key AND data !). The order of the elements is not a criterium.



   -- Iterators --
   -- ========= --

   generic
      with procedure Action
                 ( Key      : in     Key_Type;
                   Data     : in out Data_Type;
                   Continue : in out Boolean );
   procedure Visit
         ( Hash_Table : in     Hash_Table_Type );
   -- Procedure 'Visit' visits every element of the mentioned table
   -- and executes the procedure 'Action' on each of the stored data.
   -- The boolean 'Continue' specifies if you want to proceed to the
   -- next entry or if you want to stop visiting. As long as you do
   -- not modify its value within 'Action', its value remains TRUE.



private

   type Key_Access is access Key_Type;

   --:dbpool Key_Pool : GNAT.Debug_Pools.Debug_Pool;

   --:dbpool for Key_Access'Storage_Pool use Key_Pool;


   type Element_Type is record
        Key          : Key_Access;
        Data         : Data_Access;
        First        : Integer;
        Next         : Integer;
   end record;
   --

   type Hash_Array is array (Natural range <>) of Element_Type;

   type Hash_Array_Access is access Hash_Array;

   type Hash_Table_Type is new Ada.Finalization.Controlled with
      record
        Table   : Hash_Array_Access;
        Counter :  Natural := 0;
      end record;
   -- 'Counter' is the number of elements stored in the table.


   function Hash_2
         ( Number     : in     Natural;
           Hash_Table : in     Hash_Table_Type )
      return Positive;
   -- is used as the second hash function

   procedure Initialize
         ( Object : in out Hash_Table_Type );

   procedure Finalize
         ( Object : in out Hash_Table_Type );

   procedure Adjust
         ( Object : in out Hash_Table_Type );

end Hash_G;








