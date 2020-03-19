-- PROGRAM FOR GENERATING A POSTCRIPT IMAGE OF THE MANDELBROT SET
   --------------------------------------------------------------

-- Creation : 21-OCT-1989 by Mats Weber.


-- Format of input file:

-- <x_min> <x_max>
-- <y_min> <y_max>
-- <iteration_limit>
-- <size_x> <size_y>  [inches]
-- <dots_per_inch>

with Mandelbrot_Set,
     Text_IO,
     Number_Images,
     Character_Handler;

procedure Postscript_Mandelbrot is
-------------------------------

   type Real is digits 15;

   type Iterations is range 0..32000;

   package Mandelbrot is new Mandelbrot_Set(Real, Iterations);

   type Postscript_Units is digits 12;

   package Real_Text_IO             is new Text_IO.Float_IO(Real);
   package Iterations_Text_IO       is new Text_IO.Integer_IO(Iterations);
   package Postscript_Units_Text_IO is new Text_IO.Float_IO(Postscript_Units);


   Dots_Per_Inch    : Postscript_Units;
   Picas_Per_Inch   : constant Postscript_Units := 72.0;
   Max_Line_Length  : constant := 80;


   Bounds           : Mandelbrot.Rectangle_Bounds;
   Limit            : Iterations;

   Image_Size_X,
   Image_Size_Y     : Postscript_Units;


   function Image is new Number_Images.Integer_Image(Integer);
   function Image is new Number_Images.Float_Image(Postscript_Units);

begin
   Real_Text_IO.Get(Bounds.Min.X);
   Real_Text_IO.Get(Bounds.Max.X);
   Text_IO.Skip_Line;
   Real_Text_IO.Get(Bounds.Min.Y);
   Real_Text_IO.Get(Bounds.Max.Y);
   Text_IO.Skip_Line;
   Iterations_Text_IO.Get(Limit);
   Text_IO.Skip_Line;
   Postscript_Units_Text_IO.Get(Image_Size_X);
   Postscript_Units_Text_IO.Get(Image_Size_Y);
   Text_IO.Skip_Line;
   Postscript_Units_Text_IO.Get(Dots_Per_Inch);
   Text_IO.Skip_Line;
   --
   declare

      type Byte is range 0..2 ** 8 - 1;

      Powers_Of_Two    : constant array (0..7) of Byte := (2 ** 0,
                                                           2 ** 1,
                                                           2 ** 2,
                                                           2 ** 3,
                                                           2 ** 4,
                                                           2 ** 5,
                                                           2 ** 6,
                                                           2 ** 7);

      Dots_X              : constant Natural := Natural(Image_Size_X * Dots_Per_Inch);
      Dots_Y              : constant Natural := Natural(Image_Size_Y * Dots_Per_Inch);

      X_Step              : constant Real := (Bounds.Max.X - Bounds.Min.X) / Real(Dots_X - 1);
      Y_Step              : constant Real := (Bounds.Max.Y - Bounds.Min.Y) / Real(Dots_Y - 1);

      Current_Y           : Real;
      Current_Byte        : Byte := 0;
      Current_Iterations  : Iterations;
      Postscript          : Text_IO.File_Type;


      function Image (Value : Byte) return String is
      begin
         return Character_Handler.Digit_Image(Character_Handler.Digit_Number(Value / 16)) &
                Character_Handler.Digit_Image(Character_Handler.Digit_Number(Value mod 16));
      end Image;

   begin
      Text_IO.Create(Postscript, Name => "Mandelbrot.PS");
      Text_IO.Set_Output(Postscript);
      Text_IO.Put_Line("/pixelline " & Image((Dots_X - 1) / 8 + 1) & " string def");
      Text_IO.New_Line;
      Text_IO.Put_Line("/imagemandelbrot");
      Text_IO.Put_Line("   {" & Image(Dots_X) & ' ' & Image(Dots_Y) &
                       " 1 [" & Image(Dots_X) & " 0 0 " & Image(Dots_Y) & " 0 0]");
      Text_IO.Put_Line("      {currentfile pixelline readhexstring pop}");
      Text_IO.Put_Line("      image");
      Text_IO.Put_Line("   } def");
      Text_IO.New_Line;
      Text_IO.Put_Line("36 36 translate");
      Text_IO.Put_Line(Image(Image_Size_X * Picas_Per_Inch, Aft => 0) & ' ' &
                       Image(Image_Size_Y * Picas_Per_Inch, Aft => 0) & " scale");
      Text_IO.New_Line;
      Text_IO.Put_Line("imagemandelbrot");
      Text_IO.New_Line;
      --
      Text_IO.Set_Line_Length(To => Max_Line_Length);
      for J in 0..Dots_Y - 1 loop
         Current_Y := Bounds.Min.Y + Real(J) * Y_Step;
         for I in 0..Dots_X - 1 loop
            Current_Iterations := Mandelbrot.Number_Of_Iterations
                                     (Location => (X => Bounds.Min.X + Real(I) * X_Step,
                                                   Y => Current_Y),
                                      Limit    => Limit);
            if Current_Iterations /= 0 and then Current_Iterations mod 2 /= 0 then
               Current_Byte := Current_Byte + Powers_Of_Two(7 - I mod 8);
            end if;
            if I mod 8 = 7 then
               Text_IO.Put(Image(Current_Byte));
               Current_Byte := 0;
            end if;
         end loop;
         if Dots_X mod 8 /= 0 then
            Text_IO.Put(Image(Current_Byte));
            Current_Byte := 0;
         end if;
      end loop;
      Text_IO.Set_Line_Length(To => Text_IO.Unbounded);
      --
      Text_IO.New_Line(2);
      Text_IO.Put_Line("showpage");
      Text_IO.Close(Postscript);
      Text_IO.Set_Output(Text_IO.Standard_Output);
   end;
end Postscript_Mandelbrot;
