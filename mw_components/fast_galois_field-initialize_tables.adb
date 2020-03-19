-- GENERIC PACKAGE FOR HANDLING GALOIS FIELDS (GF(p^n))
   ----------------------------------------------------

-- Creation : 21-JAN-1987 by Mats Weber.


with Galois_Field,
     Dynamic_Arrays;

separate(Fast_Galois_Field)

procedure Initialize_Tables is
---------------------------

  package GF is new Galois_Field(P, N);

  package GF_ZpZ         renames GF.Base_ZpZ_Field;
  package GF_Polynomials renames GF.Base_ZpZ_Field.ZpZ_Polynomials;


  function Smaller (A, B : GF.Element) return Boolean;

  package GF_Dynamic_Arrays is new Dynamic_Arrays (Index_Type     => GF.Element,
                                                   Component_Type => Element,
                                                   "<"            => Smaller,
                                                   Count          => Integer );


  Lambda           : constant GF.Element := GF.A_Generator;

  Slow_To_Fast     : GF_Dynamic_Arrays.Dynamic_Array;
  Fast_To_Slow     : array (Element) of GF.Element;


  function Smaller (A, B : GF.Element) return Boolean is

    type GF_ZpZ_Array is array (Natural range <>) of GF_ZpZ.ZpZ;
    --% this declaration can be removed if AI-00398 is implemented.

  begin
    return GF_ZpZ_Array(GF.To_Polynomial(A)) < GF_ZpZ_Array(GF.To_Polynomial(B));
    --% return GF.TO_POLYNOMIAL(A) < GF.TO_POLYNOMIAL(B);
  end Smaller;


begin
  -- Set up the conversion tables between both representations of GF(q).
  GF_Dynamic_Arrays.Assign(Slow_To_Fast, Index => GF.Zero, Value => Zero);
  Fast_To_Slow(Zero) := GF.Zero;
  declare

    A_GF : GF.Element := GF.One;

  begin
    for A in Nonzero_Element loop
      GF_Dynamic_Arrays.Assign(Slow_To_Fast, Index => A_GF, Value => A);
      Fast_To_Slow(A) := A_GF;
      A_GF := GF."*"(A_GF, Lambda);
    end loop;
  end;
  -- Calculate addition and conversion tables
  for A in Plus_One_Table'Range loop
    Plus_One_Table(A) := GF_Dynamic_Arrays.Component(Slow_To_Fast,
                                                     Index => GF."+"(Fast_To_Slow(A),
                                                                     GF.One));
  end loop;
  for A in Element loop
    if GF.In_ZpZ(Fast_To_Slow(A)) then
      To_Element_Table(ZpZ(GF.To_ZpZ(Fast_To_Slow(A)))) := A;
      To_ZpZ_Table(A) := (Is_In_ZpZ => True,
                          ZpZ_Value => ZpZ(GF.To_ZpZ(Fast_To_Slow(A))));
    end if;
  end loop;
  GF_Dynamic_Arrays.Destroy(Slow_To_Fast);
  -- Initialize variables used by functions GF_POLYNOMIAL and TO_POLYNOMIAL.
  declare

    function To_ZpZ_Polynomial (P : GF_Polynomials.Polynomial) return Polynomial is

      Result : Polynomial(P'Range);

    begin
      for J in P'Range loop
        Result(J) := ZpZ(P(J));
      end loop;
      return Result;
    end;

  begin
    The_GF_Polynomial := To_ZpZ_Polynomial(GF.GF_Polynomial);
    Assign(Generator_Poly, To_ZpZ_Polynomial(GF.To_Polynomial(Lambda)));
  end;
end Initialize_Tables;
