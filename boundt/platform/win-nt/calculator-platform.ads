-- Calculator.Platform (decl)
--
-- Platform-dependent Calculator operations.
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
-- $Date: 2015/10/25 07:30:55 $
--
-- $Log: calculator-platform.ads,v $
-- Revision 1.2  2015/10/25 07:30:55  niklas
-- Moved to free licence.
--
-- Revision 1.1  2001-08-24 11:45:37  holsti
-- First version.
--


with Exec_Cmd;


package Calculator.Platform is


   Program : constant String := "oc";
   --
   -- The default name of the calculator program.
   -- The actual name used in the command that starts the calculator
   -- may be modified by a Bound-T command-line option.


   function Start (
      Program    : String;
      Input_Log  : String;
      Output_Log : String)
   return Exec_Cmd.Execution_T;
   --
   -- Starts a calculator instance.
   --
   -- Using  :
   --    Program, the name of the calculator program (e.g. "oc").
   --    Input_Log, the name of the file for logging input (or null).
   --    Output_Log, the name of the file for logging output (or null).
   --
   -- Giving : calculator in execution.


end Calculator.Platform;

