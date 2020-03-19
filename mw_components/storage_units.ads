-- Definition of the storage units sizes of the machine
   ----------------------------------------------------

-- Revision : 14-Nov-1991 by Mats Weber, renamed to Storage_Units.
-- Revision : 25-MAR-1986 by Mats Weber, added QUADWORD and OCTAWORD.

-- Creation : 18-AUG-1986 by Mats Weber.


package Storage_Units is
---------------------

  Byte_Bits     : constant := 8;
  Word_Bits     : constant := 2 * Byte_Bits;
  Longword_Bits : constant := 2 * Word_Bits;
  Quadword_Bits : constant := 2 * Longword_Bits;
  Octaword_Bits : constant := 2 * Quadword_Bits;

  Word_Bytes     : constant := Word_Bits/Byte_Bits;
  Longword_Bytes : constant := Longword_Bits/Byte_Bits;
  Quadword_Bytes : constant := Quadword_Bits/Byte_Bits;
  Octaword_Bytes : constant := Octaword_Bits/Byte_Bits;

end Storage_Units;
