-- Arithmetic.Algebra.Opt (decl)
--
-- Options related to algebraic parts of the arithmetic analysis.
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
-- $Revision: 1.3 $
-- $Date: 2015/10/24 19:36:46 $
--
-- $Log: arithmetic-algebra-opt.ads,v $
-- Revision 1.3  2015/10/24 19:36:46  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-08-31 04:23:33  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.1  2006/02/27 09:54:35  niklas
-- First version.
--


with Options.Bool;


package Arithmetic.Algebra.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Combine_Terms_Opt : aliased Options.Bool.Option_T (Default => True);
   --
   -- Whether to simplify arithmetic expressions by combining
   -- similar terms during the constant propagation analysis.
   --
   Combine_Terms : Boolean renames Combine_Terms_Opt.Value;


end Arithmetic.Algebra.Opt;
