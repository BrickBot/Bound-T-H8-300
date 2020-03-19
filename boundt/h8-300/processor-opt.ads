-- Processor.Opt (decl)
--
-- Options for H8/300 processors.
--
-- Author: Niklas Holsti
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
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-opt.ads,v $
-- Revision 1.3  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2015/10/26 22:01:04  niklas
-- Updated to use current Options services.
--
-- Revision 1.1  2005/01/26 19:36:07  niklas
-- First version.
--


with H8_300;
with Options;
with Options.Nat;


package Processor.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options here declared.


   package Memory_Location_Valued is new Options.Discrete_Valued (
      Value_Type  => H8_300.Memory_Location_T,
      Value_Image => H8_300.Memory_Location_T'Image);
   --
   -- Options choosing between Internal or External memory.


   Stack_Location_Opt : aliased Memory_Location_Valued.Option_T (
      Default => H8_300.Internal);
   --
   -- The assumed location of the stack.
   -- This determines the duration (number of clock states) of a Stack
   -- Operation cycle and any data-access cycle using SP = R7 as the address.
   --
   Stack_Location : H8_300.Memory_Location_T renames Stack_Location_Opt.Value;


   Read_Wait_Opt : aliased Options.Nat.Option_T (Default => 0);
   --
   -- The number of wait-states for reading external memory.
   --
   Read_Wait : Natural renames Read_Wait_Opt.Value;


   Write_Wait_Opt :  aliased Options.Nat.Option_T (Default => 0);
   --
   -- The number of wait-states for writing external memory.
   --
   Write_Wait : Natural renames Write_Wait_Opt.Value;


end Processor.Opt;
