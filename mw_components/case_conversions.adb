-- GENERIC STRING CASE CONVERSIONS
   -------------------------------

-- Creation : 17-NOV-1989 by Mats Weber.


package body Case_Conversions is
-----------------------------

   procedure Upper_Case (The_String : in out String) is
   begin
      for I in The_String'Range loop
         The_String(I) := Upper_Case(The_String(I));
      end loop;
   end Upper_Case;

   procedure Lower_Case (The_String : in out String) is
   begin
      for I in The_String'Range loop
         The_String(I) := Lower_Case(The_String(I));
      end loop;
   end Lower_Case;


   function Upper_Case (Of_String : String) return String is

      Result : String(Of_String'Range);

   begin
      for I in Result'Range loop
         Result(I) := Upper_Case(Of_String(I));
      end loop;
      return Result;
   end Upper_Case;

   function Lower_Case (Of_String : String) return String is

      Result : String(Of_String'Range);

   begin
      for I in Result'Range loop
         Result(I) := Lower_Case(Of_String(I));
      end loop;
      return Result;
   end Lower_Case;

end Case_Conversions;
