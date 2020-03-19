-- GENERIC PACKAGE FOR RANDOM GENERATION
   -------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Mon Sep  8 12:04:17 1997
-- Update Count    : 2

-- Revision : 20-SEP-1985 by Mats Weber, use package CPU for RANDOMIZE.
-- Revision : 16-SEP-1985 by Mats Weber, renamed INT_RANDOM to UNIFORM.
-- Revision : 11-DEC-1985 by Mats Weber.

-- Creation : 13-JUL-1985 by Mats Weber.


with Ada.Numerics.Generic_Elementary_Functions,
     Truncation_Functions;

package body Random_Numbers is
---------------------------

  New_Draw    : Boolean := True; -- used by NORMAL
  Next_Normal : Real;

  package Real_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Real);

  function Floor is new Truncation_Functions.Float_Floor(Generator_Real, Int);


  function Uniform (First, Last : Int) return Int is
  begin
    return Floor(Generator_Real(First) +
                 Generator_Uniform * Generator_Real(Last - First + 1));
  end Uniform;


  function Enumeration_Uniform return Enumeration is
  begin
    return Enumeration'Val(Uniform(First => Enumeration'Pos(Enumeration'First),
                                   Last  => Enumeration'Pos(Enumeration'Last)));
  end Enumeration_Uniform;


  function Uniform return Real_0_1 is
  begin
    return Real(Generator_Uniform);
  end Uniform;


  function Uniform (First, Last : Real) return Real is
  begin
    return First + (Last - First) * Uniform;
  end Uniform;


  function Normal return Real is
  begin
    if New_Draw then
      New_Draw := False;
      declare

        Z1 : constant Real := 1.0 - Uniform;
        Z2 : constant Real := Uniform;

        use Ada.Numerics,
            Real_Elementary_Functions;

        Square_Root : constant Real := Sqrt(-2.0 * Log(Z1));
        Trig_Arg    : constant Real := 2.0 * Pi * Z2;

      begin
        Next_Normal := Square_Root * Sin(Trig_Arg);
        return Square_Root * Cos(Trig_Arg);
      end;
    else
      New_Draw := True;
      return Next_Normal;
    end if;
  end Normal;


  function Exponential return Real is
  begin
    return -Real_Elementary_Functions.Log(1.0 - Uniform);
  end Exponential;


  function Probability (P : Real_0_1) return Boolean is
  begin
    return Uniform <= P;
  end;


  procedure Reset is
  begin
    Reset_Generator;
  end;


  procedure Randomize is
  begin
    Randomize_Generator;
  end;

end Random_Numbers;
