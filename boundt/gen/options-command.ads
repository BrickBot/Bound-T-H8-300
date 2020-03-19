-- Options.Command (decl)
--
-- Handling options (small inputs) from the command-line.
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: options-command.ads,v $
-- Revision 1.3  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.2  2014/06/01 10:33:38  niklas
-- Argument_Error is now the same as Options.Refused.
--
-- Revision 1.1  2011-08-31 04:17:13  niklas
-- Added for BT-CH-0222: Option registry. Option -dump. External help files.
--


package Options.Command is


   Argument_Error : exception renames Options.Refused;
   --
   -- A syntactical or semantic error in the command-line.
   -- Possibly raised by Get_From_Command.


   procedure Get_From_Command (
      Took_Action   : out Boolean;
      Next_Argument : out Positive);
   --
   -- Scans the command line and parses and interprets all the
   -- options, that is, all leading arguments that start with the
   -- character '-'.
   --
   -- Stores the option values in the various *.Opt packages.
   --
   -- Took_Action
   --    Whether some useful action (not just option storage) was
   --    taken in response to options. If so, an error should not
   --    be signalled if there are no more arguments.
   -- Next_Argument
   --    The number of the first unprocessed argument. If this
   --    is > Ada.Command_Line.Argument_Count, then all arguments
   --    were processed.
   --
   -- Raises Argument_Error on any error.


   procedure Advise_Help;
   --
   -- Gives advice on how to find help for the command-line syntax.


   procedure Describe;
   --
   -- Describes the possible options and arguments on standard output.


end Options.Command;
