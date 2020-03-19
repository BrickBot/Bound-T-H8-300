-- Assertions.Own.Text (decl)
--
-- Textual display of assertions.
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: assertions-own-text.ads,v $
-- Revision 1.2  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.1  2006-05-27 21:26:37  niklas
-- First version for BT-CH-0020.
--


package Assertions.Own.Text is


   function Image (Item : Time_Bound_T) return String;


   function Image (Item : Fact_T) return String;


   function Image (Item : Part_Name_T) return String;


   function Image (Item : Parts_T) return String;
   --
   -- The image omits the Predicate. Use the Put procedure below.


   procedure Put (
      Item   : Feature_T;
      Indent : Natural;
      Outer  : Goals_T);
   --
   -- Displays this feature Item on standard output, using a given
   -- amount of Indentation, and recursively displaying the contained
   -- or containing parts but without recursing to the predicates
   -- already in the Outer list.


   procedure Put (
      Item   : Part_Predicate_Ref;
      Indent : Natural;
      Outer  : Goals_T);
   --
   -- Displays this predicate Item on standard output, using a given
   -- amount of Indentation, and recursively displaying the contained
   -- or containing parts but without recursing to the predicates
   -- already in the Outer list.


   procedure Put (Item : Assertion_T);
   --
   -- Displays this assertion Item on standard output.


   procedure Put (Item : Assertion_Bag_T);
   --
   -- Displays all the assertions in the bag, on standard output.


end Assertions.Own.Text;
