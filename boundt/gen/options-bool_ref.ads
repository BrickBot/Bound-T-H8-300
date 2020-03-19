-- Options.Bool_Ref (decl)
--
-- Boolean-valued options that are implemented by an existing Boolean
-- variable that is referenced from the Option_T.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-bool_ref.ads,v $
-- Revision 1.2  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2012-01-21 11:50:39  niklas
-- First version, for BT-CH-0225.
--


with Options.Bool;


package Options.Bool_Ref is


   type Boolean_Ref is access all Boolean;
   --
   -- Refers to some (aliased) Boolean variable.


   type Option_T (Variable : Boolean_Ref; Default : Boolean)
   is new Options.Bool.Option_T (Default => Default)
   with null record;
   --
   -- An option with a Boolean value, with a given Default value,
   -- that is implemented by an existing Variable. Setting or resetting
   -- this option changes the value of the Variable accordingly.


   overriding
   procedure Reset (Option : access Option_T);


   overriding
   procedure Set (
      Option : access Option_T;
      Value  : in     String);


end Options.Bool_Ref;
