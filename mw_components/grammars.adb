-- GENERIC PACKAGE FOR HANDLING GRAMMARS
   -------------------------------------

-- Creation : 22-FEB-1989 by Mats Weber.


with Min_Max_Functions;

package body Grammars is
---------------------

   function "<" (Left, Right : Symbol) return Boolean is
   begin
      if Left.Kind /= Right.Kind then
         return Left.Kind < Right.Kind;
      else
         case Left.Kind is
            when Terminal =>
               return Left.Terminal_Value < Right.Terminal_Value;
            when Nonterminal =>
               return Left.Nonterminal_Value < Right.Nonterminal_Value;
         end case;
      end if;
   end "<";

   function ">" (Left, Right : Symbol) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<=" (Left, Right : Symbol) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function ">=" (Left, Right : Symbol) return Boolean is
   begin
      return Right <= Left;
   end ">=";


   function "<" (Left, Right : Symbol_String) return Boolean is

      function Min is new Min_Max_Functions.Minimum(Natural);

   begin
      for I in 1..Min(Left'Length, Right'Length) loop
         if Left(Left'First + I - 1) /= Right(Right'First + I - 1) then
            return Left(Left'First + I - 1) < Right(Right'First + I - 1);
         end if;
      end loop;
      return Left'Length < Right'Length;
   end "<";

   function ">" (Left, Right : Symbol_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<=" (Left, Right : Symbol_String) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   function ">=" (Left, Right : Symbol_String) return Boolean is
   begin
      return Right <= Left;
   end ">=";


   function Left_Part (Of_Production : Production) return Nonterminal_Symbol is
   begin
      return Of_Production.Left;
   end Left_Part;


   function Right_Part (Of_Production : Production) return Symbol_String is
   begin
      return Symbol_Varying_Text.To_String(Of_Production.Right);
   end Right_Part;


   procedure Convert_To_Greibach_Normal_Form (The_Grammar : in out Grammar) is separate;

end Grammars;
