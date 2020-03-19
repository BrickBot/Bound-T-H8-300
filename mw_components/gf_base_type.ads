-- PACKAGE DEFINING THE BASE TYPE FOR NUMBERS IN THE RANGE 0..Q IN GALOIS_FIELD
   ----------------------------------------------------------------------------

-- Creation : 17-DEC-1987 by Mats Weber.


package GF_Base_Type is
--------------------

  type GF_Integer is range -(2**31 - 1) .. 2**31 - 1;

  subtype GF_Natural  is GF_Integer range 0..GF_Integer'Last;
  subtype GF_Positive is GF_Integer range 1..GF_Integer'Last;

end GF_Base_Type;
