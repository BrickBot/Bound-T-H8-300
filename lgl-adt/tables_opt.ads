-- Tables_Opt
--
-- Command-line options for the Table_Of_Xxx modules as used in Bound-T.
--
-- This is not a child package of Table_Of_xxx because those are generic
-- while we want options to apply to all tables.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
-- Copyright (c) 2007 Tidorum Ltd.
--
-- $RCSfile: tables_opt.ads,v $
-- $Revision: 1.1 $
-- $Date: 2007-02-15 11:12:16 $
-- $Name:  $
--
-- $Log: tables_opt.ads,v $
-- Revision 1.1  2007-02-15 11:12:16  Niklas
-- BT-CH-0045.
--


package Tables_Opt is


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Tables_Opt;
