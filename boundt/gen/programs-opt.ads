-- Programs.Opt (decl)
--
-- Command-line options that control the management of Program,
-- Subprogram and Call objects.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 19:36:51 $
--
-- $Log: programs-opt.ads,v $
-- Revision 1.5  2015/10/24 19:36:51  niklas
-- Moved to free licence.
--
-- Revision 1.4  2011-08-31 04:23:34  niklas
-- BT-CH-0222: Option registry. Option -dump. External help files.
--
-- Revision 1.3  2007/04/29 12:03:08  niklas
-- Added Qualify_Names.
--
-- Revision 1.2  2007/01/25 21:25:18  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/05/01 08:45:41  niklas
-- First version.
--


with Options.Bool;


package Programs.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options.


   Trace_Subprograms_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to trace the creation and some uses of Subprogram
   -- objects.
   --
   Trace_Subprograms : Boolean renames Trace_Subprograms_Opt.Value;


   Qualify_Names_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether the display subprogram names qualified with the scope
   -- that contains the subprogram (in the Symbol Table sense).
   -- This overrides a False value of the parameter Qualified in
   -- the Name function(s). It does not not override a True value
   -- of this parameter.
   --
   Qualify_Names : Boolean renames Qualify_Names_Opt.Value;


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory.


end Programs.Opt;
