-- Calling.Opt (decl)
--
-- Command-line options for the analysing calls and calling protocols.
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
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: calling-opt.ads,v $
-- Revision 1.3  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.2  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.1  2005/02/23 09:05:15  niklas
-- BT-CH-0005.
--


with Options.Bool;


package Calling.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Protocols_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the bounding of dynamic calling protocols.
   --
   Trace_Protocols : Boolean renames Trace_Protocols_Opt.Value;


   Trace_Parameters_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the bounds on parameter values, derived from
   -- the calling context, for call-dependent bounding.
   --
   Trace_Parameters : Boolean renames Trace_Parameters_Opt.Value;


end Calling.Opt;
