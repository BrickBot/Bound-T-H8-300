-- GENERIC PACKAGE FOR STUDYING THE MANDELBROT SET
   -----------------------------------------------

-- Creation :  7-OCT-1988 by Mats Weber.


generic
   type Real is digits <>;
   type Iterations is range <>;  -- must include 0
package Mandelbrot_Set is
----------------------

   type Point is
      record
         X, Y : Real;
      end record;

   type Rectangle_Bounds is
      record
         Min,
         Max : Point;
      end record;


   type Mandelbrot_Set_Image is array (Natural range <>, Natural range <>) of Iterations;


   function Number_Of_Iterations (Location : Point;
                                  Limit    : Iterations) return Iterations;
      -- Returns the smallest value n for which Z(n) lies outside B(0,2),
      -- where Z(i) is defined by the series Z(i+1) = Z(i) + Point, Z(0) = 0.
      -- If Z(Limit) is in B(0,2), then 0 is returned.

   procedure Compute_Image (Bounds : in Rectangle_Bounds;
                            Limit  : in Iterations;
                            Image  : out Mandelbrot_Set_Image);

   procedure Recompute_Image (Bounds     : in Rectangle_Bounds;
                              New_Limit  : in Iterations;
                              Image      : in out Mandelbrot_Set_Image);
      -- Calculates only the points for which Image(I, J) = 0 or Image(I, J) > New_Limit.
      -- Bounds must be the same as given in Compute_Image.


   pragma Inline(Number_Of_Iterations);

end Mandelbrot_Set;
