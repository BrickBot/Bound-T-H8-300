-- Bags_Opt
--
-- Command-line options for the Bags module as used in Bound-T.
--
-- This is not a child package of Bags because Bags is generic
-- while we want options to apply to all Bags.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
-- Copyright (c) 2007 Tidorum Ltd.
--
-- $RCSfile: bags_opt.ads,v $
-- $Revision: 1.1 $
-- $Date: 2007-02-15 10:41:21 $
-- $Name:  $
--
-- $Log: bags_opt.ads,v $
-- Revision 1.1  2007-02-15 10:41:21  Niklas
-- BT-CH-0045.
--


package Bags_Opt is


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Bags_Opt;
