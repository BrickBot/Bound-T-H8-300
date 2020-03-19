-- PACKAGE DEFINING THE BASE TYPE FOR TYPE ELEMENT IN FAST_GALOIS_FIELD
   --------------------------------------------------------------------

-- Revision : 17-DEC-1987 by Mats Weber, renamed GF_INTEGER to FAST_GF_INTEGER.

-- Creation : 20-JAN-1987 by Mats Weber.


package Fast_GF_Base_Type is
-------------------------

  type Fast_GF_Integer is range -32767..32767;

  subtype Fast_GF_Natural  is Fast_GF_Integer range 0..Fast_GF_Integer'Last;
  subtype Fast_GF_Positive is Fast_GF_Integer range 1..Fast_GF_Integer'Last;

end Fast_GF_Base_Type;
