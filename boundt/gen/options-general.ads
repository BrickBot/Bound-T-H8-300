-- Options.General (decl)
--
-- Options that are common to many analyses and modules in Bound-T,
-- although each analysis or module may interpret and apply the
-- option differently according to its specific nature.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-general.ads,v $
-- Revision 1.2  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2013-02-05 20:07:47  niklas
-- First version.
--


with Options.Bool;


package Options.General is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Resolution_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the process of resolving dynamically addressed code
   -- (dynamic control flow) and data (dynamic data pointers).
   --
   Trace_Resolution : Boolean renames Trace_Resolution_Opt.Value;


end Options.General;
