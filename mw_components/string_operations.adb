-- GENERIC PACKAGE FOR STRING HANDLING
   -----------------------------------

-- Revision : 31-JUL-1986 by Mats Weber, optimized LOCATE(STRING, STRING, SIDE)
--                                       using slices.
-- Creation :  5-JUL-1986 by Mats Weber.


package body String_Operations is
------------------------------

   function String_1 (Of_String : String) return String is

      subtype String_1_Length is String(1..Of_String'Length);

   begin
      return String_1_Length(Of_String);
   end String_1;


   function Scan (Within : String;
                  From   : Side := Left) return Natural is
   begin
      case From is
         when Left =>
            for I in Within'Range loop
               if Condition(Within(I)) then
                  return I;
               end if;
            end loop;
            return Within'Last + 1;
         when Right =>
            for I in reverse Within'Range loop
               if Condition(Within(I)) then
                  return I;
               end if;
            end loop;
            return Within'First - 1;
      end case;
   end Scan;


   procedure Transformation (On_String : in out String) is
   begin
      for I in On_String'Range loop
         Transform(On_String(I));
      end loop;
   end Transformation;


   function Transformation_Function (Of_String : String) return String is

      Result : String(Of_String'Range);

   begin
      for I in Result'Range loop
         Result(I) := Transformed(Of_String(I));
      end loop;
      return Result;
   end Transformation_Function;


   function Locate (Pattern : Character;
                    Within  : String;
                    From    : Side := Left) return Natural is
   begin
      case From is
         when Left =>
            for I in Within'Range loop
               if Within(I) = Pattern then
                  return I;
               end if;
            end loop;
            return Within'Last + 1;
         when Right =>
            for I in reverse Within'Range loop
               if Within(I) = Pattern then
                  return I;
               end if;
            end loop;
            return Within'First - 1;
      end case;
   end Locate;


   function Locate (Pattern : String;
                    Within  : String;
                    From    : Side := Left) return Natural is
   begin
      case From is
         when Left =>
            for I in Within'First..Within'Last - Pattern'Length + 1 loop
               if Within(I..I + Pattern'Length - 1) = Pattern then
                  return I;
               end if;
            end loop;
            return Within'Last + 1;
         when Right =>
            for I in reverse Within'First..Within'Last - Pattern'Length + 1 loop
               if Within(I..I + Pattern'Length - 1) = Pattern then
                  return I;
               end if;
            end loop;
            return Within'First - 1;
      end case;
   end Locate;


   function Located (Pattern : Character; Within : String) return Boolean is
   begin
      return Locate(Pattern, Within) <= Within'Last;
   end;


   function Located (Pattern : String; Within : String) return Boolean is
   begin
      return Locate(Pattern, Within) <= Within'Last;
   end;


   function Delete (Pattern : Character;
                    Within  : String;
                    From    : Side := Left) return String is

      Pattern_Pos : constant Natural := Locate(Pattern, Within, From);

   begin
      return Within(Within'First..Pattern_Pos - 1) &
             Within(Pattern_Pos + 1..Within'Last);
   end Delete;


   function Delete (Pattern : String;
                    Within  : String;
                    From    : Side := Left) return String is

      Pattern_Pos : constant Natural := Locate(Pattern, Within, From);

   begin
      return Within(Within'First..Pattern_Pos - 1) &
             Within(Pattern_Pos + Pattern'Length..Within'Last);
   end Delete;


   function "-" (Left : String; Right : Character) return String is

      Pattern_Pos : Natural;
      Result      : String(Left'Range) := Left;
      Last        : Integer range Result'First - 1..Result'Last := Result'Last;

   begin
      loop
         Pattern_Pos := Locate(Right, Within => Result(Result'First..Last));
         exit when Pattern_Pos > Last;
         Result(Pattern_Pos..Last - 1) := Result(Pattern_Pos + 1..Last);
         Last := Last - 1;
      end loop;
      return Result(Result'First..Last);
   end "-";


   function "-" (Left : String; Right : String) return String is

      Pattern_Pos : Natural;
      Result      : String(Left'Range) := Left;
      Last        : Integer range Result'First - 1..Result'Last := Result'Last;

   begin
      if Right'Length /= 0 then
         loop
            Pattern_Pos := Locate(Right, Within => Result(Result'First..Last));
            exit when Pattern_Pos > Last;
            Result(Pattern_Pos..Last - Right'Length) :=
               Result(Pattern_Pos + Right'Length..Last);
            Last := Last - Right'Length;
         end loop;
      end if;
      return Result(Result'First..Last);
   end "-";

end String_Operations;
