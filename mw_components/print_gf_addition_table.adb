-- PROCEDURE TO PRINT THE ADDITION TABLE USED IN FAST_GALOIS_FIELD
   ---------------------------------------------------------------

-- Creation : 25-MAR-1987 by Mats Weber.


with Text_IO,
     User_Interface,
     ZpZ_Base_Type,
     GF_Base_Type,
     Fast_Galois_Field;

use Text_IO,
    GF_Base_Type;

procedure Print_GF_Addition_Table is
---------------------------------

   Left_Margin  : constant := 10;
   Column_Width : constant := 4;

   function ZpZ_Integer_Answer is
      new User_Interface.Integer_Answer(ZpZ_Base_Type.ZpZ_Integer);

   function Positive_Answer is
      new User_Interface.Integer_Answer(Positive);

   P : constant ZpZ_Base_Type.ZpZ_Integer := ZpZ_Integer_Answer("Enter P : ");
   N : constant Positive                  := Positive_Answer("Enter N : ");

   package GF_P_N is new Fast_Galois_Field(P, N);

   use GF_P_N;

   package GF_Integer_Text_IO is
      new Text_IO.Integer_IO(GF_Base_Type.GF_Integer);

   use GF_Integer_Text_IO;

begin
   for J in 0..Q - 2 loop
      Set_Col(Left_Margin + Count(J) * Column_Width);
      Put(J, Width => Column_Width);
   end loop;
   New_Line(2);
   for I in 0..Q - 2 loop
      Put(I, Width => Left_Margin - 1);
      for J in 0..Q - 2 loop
         Set_Col(Left_Margin + Count(J) * Column_Width);
         begin
            Put(Power_Of_Generator(The_Generator ** I + The_Generator ** J),
                Width => Column_Width);
         exception
            when Zero_Is_Not_A_Power =>
               Set_Col(Col + Column_Width - 1);
               Put('*');
         end;
      end loop;
      New_Line;
   end loop;
end Print_GF_Addition_Table;
