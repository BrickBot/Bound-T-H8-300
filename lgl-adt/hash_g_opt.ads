-- Hash_G_Opt
--
-- Command-line options for the Hash_G module as used in Bound-T.
--
-- This is not a child package of Hash_G because Hash_G is generic
-- while we want options to apply to all hashes.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
-- Copyright (c) 2007 Tidorum Ltd.
--
-- $RCSfile: hash_g_opt.ads,v $
-- $Revision: 1.1 $
-- $Date: 2007-02-15 11:12:09 $
-- $Name:  $
--
-- $Log: hash_g_opt.ads,v $
-- Revision 1.1  2007-02-15 11:12:09  Niklas
-- BT-CH-0045.
--


package Hash_G_Opt is


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Hash_G_Opt;
