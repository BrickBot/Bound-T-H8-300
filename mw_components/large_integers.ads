-- GENERIC LARGE INTEGER HANDLING PACKAGE
   --------------------------------------

-- Creation : 24-MAY-1988 by Mats Weber.


generic
   type Slice_Number      is range <>;  -- Type for the value of a slice
   type Arithmetic_Number is range <>;  -- Type for calculations.
                                        -- Its range must be at least 0..(SLICE_NUMBER'LAST + 1)**2 - 1
   type Slice_Index       is range <>;  -- Type for indexing slices
package Large_Integers is
----------------------

   type Large_Integer (Length : Slice_Index := 0) is private;
   ----------------------------------------------


   procedure Assign (Object : out Large_Integer; Value : in Large_Integer);

   function Equal (X, Y : Large_Integer) return Boolean;

   function "<"  (X, Y : Large_Integer) return Boolean;
   function ">"  (X, Y : Large_Integer) return Boolean;
   function "<=" (X, Y : Large_Integer) return Boolean;
   function ">=" (X, Y : Large_Integer) return Boolean;

   function Is_Zero (X : Large_Integer) return Boolean;
   function Is_One  (X : Large_Integer) return Boolean;

   generic
      type Int is range <>;
   function Large_Integer_To_Integer (X : Large_Integer) return Int;

   function Image (X : Large_Integer) return String;


   package Variable_Length_Operations is
   ----------------------------------

      function "+" (X : Large_Integer) return Large_Integer;
      function "-" (X : Large_Integer) return Large_Integer;

      function "abs" (X : Large_Integer) return Large_Integer;

      function "+" (X, Y : Large_Integer) return Large_Integer;
      function "-" (X, Y : Large_Integer) return Large_Integer;
      function "*" (X, Y : Large_Integer) return Large_Integer;
      function "/" (X, Y : Large_Integer) return Large_Integer;

      function "mod" (X, Y : Large_Integer) return Large_Integer;
      function "rem" (X, Y : Large_Integer) return Large_Integer;

      function "**" (X : Large_Integer; Exp : Natural) return Large_Integer;

      function Zero return Large_Integer;
      function One  return Large_Integer;

      function First return Large_Integer;
      function Last  return Large_Integer;
         -- Return the smallest and greatest representable LARGE_INTEGERs.

      generic
         type Int is range <>;
      function Integer_To_Large_Integer (X : Int) return Large_Integer;

      function Value (Image : String) return Large_Integer;

   end Variable_Length_Operations;


   generic
      Length : Slice_Index;
   package Fixed_Length_Operations is
   -------------------------------

      subtype Fixed_Length_Large_Integer is Large_Integer(Length);
      ----------------------------------


      function "+" (X : Large_Integer) return Fixed_Length_Large_Integer;
      function "-" (X : Large_Integer) return Fixed_Length_Large_Integer;

      function "abs" (X : Large_Integer) return Fixed_Length_Large_Integer;

      function "+" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "-" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "*" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "/" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;

      function "mod" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "rem" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;

      function "**" (X : Large_Integer; Exp : Natural) return Fixed_Length_Large_Integer;

      function Zero return Fixed_Length_Large_Integer;
      function One  return Fixed_Length_Large_Integer;

      function First return Fixed_Length_Large_Integer;
      function Last  return Fixed_Length_Large_Integer;
         -- Return the smallest and greatest representable FIXED_LENGTH_LARGE_INTEGERs.

      generic
         type Int is range <>;
      function Integer_To_Large_Integer (X : Int) return Fixed_Length_Large_Integer;

      function Value (Image : String) return Fixed_Length_Large_Integer;

   end Fixed_Length_Operations;


   Overflow,
   Division_By_Zero  : exception;

private

   type Number_Sign is range -1..+1;

   subtype Natural_Slice_Number is Slice_Number range 0..Slice_Number'Last;

   type Slice_Array is array (Slice_Index range <>) of Natural_Slice_Number;

   type Large_Integer (Length : Slice_Index := 0) is
      record
         Sign  : Number_Sign := 0;
         Value : Slice_Array(0..Length) := (others => 0);
      end record;

end Large_Integers;
