-- OpenToken.Opt
--
-- Command-line options for the OpenToken system as used in Bound-T.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
-- Copyright (c) 2007 Tidorum Ltd.
--
-- $RCSfile: opentoken-opt.ads,v $
-- $Revision: 1.1 $
-- $Date: 2007-02-15 13:11:44 $
-- $Name:  $
--
-- $Log: opentoken-opt.ads,v $
-- Revision 1.1  2007-02-15 13:11:44  Niklas
-- BT-CH-0045.
--


package OpenToken.Opt is


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end OpenToken.Opt;
