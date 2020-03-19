-- Decoder.IAR.Opt (decl)
--
-- Options related to the IAR compiler for the Renesas H8/300.
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
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-iar-opt.ads,v $
-- Revision 1.3  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--
-- Revision 1.1  2005/05/09 15:56:14  niklas
-- First version.
--


with Options;


package Decoder.IAR.Opt is


   subtype Param_Regs_T is Natural range 0 .. 3;
   --
   -- The values for the -ur option.


   package Param_Regs_Valued is new Options.Integer_Valued (
      Value_Type => Param_Regs_T);


   Param_Regs_Opt : aliased Param_Regs_Valued.Option_T (Default => 0);
   --
   -- The -ur option value which defines the set of registers
   -- that can pass parameters.
   --
   Param_Regs : Param_Regs_T renames Param_Regs_Opt.Value;


   subtype Trash_Regs_T is Natural range 0 .. 4;
   --
   -- The values for the -uu option.


   package Trash_Regs_Valued is new Options.Integer_Valued (
      Value_Type => Trash_Regs_T);


   Trash_Regs_Opt : aliased Trash_Regs_Valued.Option_T (Default => 0);
   --
   -- The -uu option value which defines the set of "trash" registers
   -- that can be clobbered by a subprogram call.
   --
   Trash_Regs : Trash_Regs_T renames Trash_Regs_Opt.Value;


   procedure Handle_Wild_Option (
      Name  : in     String;
      Valid :    out Boolean);
   --
   -- Handles a command-line option of the form "-<Name>", when
   -- the Name is not the name of any registered option.


   procedure Finish;
   --
   -- Finishes the setting of options, after all command-line option
   -- arguments have been processed, and before reading the executable
   -- target program that will be analyzed.


end Decoder.IAR.Opt;
