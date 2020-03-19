-- PACKAGE FOR CHECKING IF A BCH CODE IS IN CANONICAL FORM
   -------------------------------------------------------

-- Creation :  3-NOV-1986 by Mats Weber.


with GF_Base_Type;

use GF_Base_Type;

package body Canonical_BCH_Codes is
--------------------------------

  function Canonical (P : ZpZ_Positive; N : Positive; Q : GF_Positive; T : Positive) return Boolean is
  begin
    if GF_Positive(T) not in 1..Q - 1 then
      raise T_Out_Of_Range;
    end if;
    for I in 1..T - 1 loop
      for K in 0..N - 1 loop
        if (GF_Positive(I) * GF_Positive(P) ** K - GF_Positive(T)) mod (Q - 1) = 0 then
          return False;
        end if;
      end loop;
    end loop;
    return True;
  end Canonical;


  function Canonical (P : ZpZ_Positive; N : Positive; T : Positive) return Boolean is
  begin
    return Canonical(P => P, N => N, Q => GF_Positive(P) ** N, T => T);
  end;


  function BCH_Bound (P : ZpZ_Positive; N : Positive; T : Positive) return Positive is

    Q : constant GF_Positive := GF_Positive(P) ** N;

  begin
    for T2 in T..Positive(Q - 1) loop
      if Canonical(P, N, Q, T2) then
        return T2;
      end if;
    end loop;
  end BCH_Bound;

end Canonical_BCH_Codes;
