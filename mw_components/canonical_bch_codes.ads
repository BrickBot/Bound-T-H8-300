-- PACKAGE FOR CHECKING IF A BCH CODE IS IN CANONICAL FORM
   -------------------------------------------------------

-- Revision : 18-DEC-1987 by Mats Weber, changed parameter types.
-- Revision : 23-FEB-1987 by Mats Weber, removed enumeration procedure.

-- Creation :  3-NOV-1986 by Mats Weber.


with ZpZ_Base_Type;

use ZpZ_Base_Type;

package Canonical_BCH_Codes is
---------------------------

  -- The BCH code BCH(p,n,t,a) is said to be in canonical form
  -- if a^t /= Fr^k(a^i) for all k = 1,...,n-1 and all i = 1,...,t-1.
  -- In this case t is the BCH bound of the code.


  function Canonical (P : ZpZ_Positive; N : Positive; T : Positive) return Boolean;
    -- Checks if the given code is in canonical form.
    -- P must be prime (this is not checked).

  function BCH_Bound (P : ZpZ_Positive; N : Positive; T : Positive) return Positive;
    -- Returns the value of T generating the same code, but such
    -- that the representation is canonical (the BCH bound).

  T_Out_Of_Range : exception;
    -- Raised by function CANONICAL if 0 < t < q=p^n is not satisfied.

end Canonical_BCH_Codes;
