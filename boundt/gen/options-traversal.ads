-- Options.Traversal (decl)
--
-- Grouped traversal of (sets of) options, for display purposes.
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: options-traversal.ads,v $
-- Revision 1.2  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


private
package Options.Traversal is


   type Visitor_T is abstract tagged null record;
   --
   -- Defines the actions to be taken for options and option groups
   -- during the traversal of an option set.


   procedure Visit (
      Visitor : in out Visitor_T;
      Option  : in     Option_Element_T)
   is abstract;
   --
   -- This Option is traversed.


   procedure Enter_Group (
      Visitor : in out Visitor_T;
      Group   : in     Group_T)
   is abstract;
   --
   -- The traversal is entering a set of options that belong to this Group.


   procedure Leave_Group (
      Visitor : in out Visitor_T;
      Group   : in     Group_T)
   is abstract;
   --
   -- The traversal is leaving a set of options that belong to this Group.


   procedure Traverse (
      List     : in     Option_List_T;
      Within   : in     Group_Set_T := Null_Group_Set;
      Synonyms : in     Boolean;
      Visitor  : in out Visitor_T'Class);
   --
   -- Traverses the given List of options in a tree order by groups
   -- and the priorities of the groups, assuming that the traversal
   -- has already "entered" Within the given groups. Inclusion of
   -- synonyms is optional.


end Options.Traversal;
