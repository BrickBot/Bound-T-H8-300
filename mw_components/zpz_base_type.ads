-- PACKAGE DEFINING THE BASE TYPE FOR TYPE ZPZ IN ZPZ_FIELD
   --------------------------------------------------------

-- Revision : 17-DEC-1987 by Mats Weber, added subtypes ZPZ_NATURAL and ZPZ_POSITIVE.

-- Creation : 15-JAN-1987 by Mats Weber.


package ZpZ_Base_Type is
---------------------

  type ZpZ_Integer is range -127..127;

  subtype ZpZ_Natural  is ZpZ_Integer range 0..ZpZ_Integer'Last;
  subtype ZpZ_Positive is ZpZ_Integer range 1..ZpZ_Integer'Last;

end ZpZ_Base_Type;
