-- GENERIC PACKAGE FOR STUDYING THE MANDELBROT SET
   -----------------------------------------------

-- Creation :  7-OCT-1988 by Mats Weber.


package body Mandelbrot_Set is
---------------------------

   function Number_Of_Iterations (Location : Point;
                                  Limit    : Iterations) return Iterations is

      X, Y,
      X_Squared, Y_Squared  : Real := 0.0;

   begin
      for I in 1..Limit loop
         Y := 2.0 * X * Y + Location.Y;
         X := X_Squared - Y_Squared + Location.X;
         X_Squared := X ** 2;
         Y_Squared := Y ** 2;
         if X_Squared + Y_Squared > 4.0 then
            return I;
         end if;
      end loop;
      return 0;
   end Number_Of_Iterations;


   procedure Compute_Image (Bounds : in Rectangle_Bounds;
                            Limit  : in Iterations;
                            Image  : out Mandelbrot_Set_Image) is

      X_Step : constant Real := (Bounds.Max.X - Bounds.Min.X) / Real(Image'Length(1) - 1);
      Y_Step : constant Real := (Bounds.Max.Y - Bounds.Min.Y) / Real(Image'Length(2) - 1);

      Current_X : Real;

   begin
      for I in Image'Range(1) loop
         Current_X := Bounds.Min.X + Real(I - Image'First(1)) * X_Step;
         for J in Image'Range(2) loop
            Image(I, J) := Number_Of_Iterations(Location => (X => Current_X,
                                                             Y => Bounds.Min.Y + Real(J - Image'First(2)) * Y_Step),
                                                Limit    => Limit);
         end loop;
      end loop;
   end Compute_Image;


   procedure Recompute_Image (Bounds     : in Rectangle_Bounds;
                              New_Limit  : in Iterations;
                              Image      : in out Mandelbrot_Set_Image) is

      X_Step : constant Real := (Bounds.Max.X - Bounds.Min.X) / Real(Image'Length(1) - 1);
      Y_Step : constant Real := (Bounds.Max.Y - Bounds.Min.Y) / Real(Image'Length(2) - 1);

      Current_X : Real;

   begin
      for I in Image'Range(1) loop
         Current_X := Bounds.Min.X + Real(I - Image'First(1)) * X_Step;
         for J in Image'Range(2) loop
            if Image(I, J) = 0 or else Image(I, J) > New_Limit then
               Image(I, J) :=
                  Number_Of_Iterations(Location => (X => Current_X,
                                                    Y => Bounds.Min.Y + Real(J - Image'First(2)) * Y_Step),
                                       Limit    => New_Limit);
            end if;
         end loop;
      end loop;
   end Recompute_Image;

end Mandelbrot_Set;
