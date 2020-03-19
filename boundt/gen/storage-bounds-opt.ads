-- Storage.Bounds.Opt (decl)
--
-- Options related to bounds on values and boundable things.
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.4 $
-- $Date: 2015/10/24 19:36:52 $
--
-- $Log: storage-bounds-opt.ads,v $
-- Revision 1.4  2015/10/24 19:36:52  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013/12/20 21:01:08  niklas
-- Made Max_List_Values(_Opt) into a registered option.
--
-- Revision 1.2  2007-01-25 21:25:18  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2005/02/16 21:11:48  niklas
-- BT-CH-0002.
--


with Options.Nat;


package Storage.Bounds.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Max_Listed_Values_Opt : aliased Options.Nat.Option_T (Default => 300);
   --
   -- The maximum number of elements that can be listed (enumerated)
   -- from the Bounds on some cell or expression. Such lists are used
   -- mainly to resolve dynamic jumps and mainly switch/case statements,
   -- where this number sets a limit on the number of case branches.
   --
   -- Without such a limit, the enumerator might try to list close to
   -- Processor.Value_T'Last values which would usually not be a good
   -- or even feasible idea.
   --
   Max_Listed_Values : Natural renames Max_Listed_Values_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Storage.Bounds.Opt;
