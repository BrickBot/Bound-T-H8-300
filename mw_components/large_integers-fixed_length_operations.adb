separate (Large_Integers)

package body Fixed_Length_Operations is
------------------------------------

   function Copy_Of (X : Large_Integer; Length : Slice_Index) return Large_Integer is
      -- Returns a copy of X with a length equal to LENGTH.
      -- X.LENGTH must be less than or equal to LENGTH.
   begin
      if X.Length > Length then
         raise Overflow;
      else
         return (Length => Length,
                 Sign   => X.Sign,
                 Value  => X.Value & (1..Length - X.Length => 0));
      end if;
   end Copy_Of;

   pragma Inline(Copy_Of);


   function "+" (X : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."+"(X), Length);
   end "+";


   function "-" (X : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."-"(X), Length);
   end "-";


   function "abs" (X : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."abs"(X), Length);
   end "abs";


   function "+" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."+"(X, Y), Length);
   end "+";


   function "-" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."-"(X, Y), Length);
   end "-";


   function "*" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."*"(X, Y), Length);
   end "*";


   function "/" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."/"(X, Y), Length);
   end "/";


   function "mod" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."mod"(X, Y), Length);
   end "mod";


   function "rem" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."rem"(X, Y), Length);
   end "rem";


   function "**" (X : Large_Integer; Exp : Natural) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations."**"(X, Exp), Length);
   end "**";


   function Zero return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => 0,
              Value  => (others => 0));
   end Zero;


   function One return Fixed_Length_Large_Integer is

      Result : Fixed_Length_Large_Integer;

   begin
      Result.Sign := +1;
      Result.Value(0) := 1;
      Result.Value(1..Length) := (others => 0);
      return Result;
   end One;


   function First return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => -1,
              Value  => (others => Slice_Number'Last));
   end First;


   function Last return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => +1,
              Value  => (others => Slice_Number'Last));
   end Last;


   function Integer_To_Large_Integer (X : Int) return Fixed_Length_Large_Integer is

      function To_Large_Integer is new Variable_Length_Operations.Integer_To_Large_Integer(Int);

   begin
      return Copy_Of(To_Large_Integer(X), Length);
   end Integer_To_Large_Integer;


   function Value (Image : String) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of(Variable_Length_Operations.Value(Image), Length);
   end Value;

end Fixed_Length_Operations;
