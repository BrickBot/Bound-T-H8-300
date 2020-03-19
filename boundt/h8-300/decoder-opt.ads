-- Decoder.Opt (decl)
--
-- Command-line options for the Renesesa H8/300 instruction decoder.
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.14 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-opt.ads,v $
-- Revision 1.14  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.13  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--
-- Revision 1.12  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.11  2008-10-29 14:46:38  niklas
-- Updated for BT-CH-0129: Choose_Device, List_Device_Names.
--
-- Revision 1.10  2008/04/29 20:09:46  niklas
-- Updated to include Set_Warn_Option.State.
--
-- Revision 1.9  2007/12/27 23:12:32  niklas
-- BT-CH-0104: S-record program format for Bound-T/H8-300.
--
-- Revision 1.8  2007/03/01 11:55:55  niklas
-- Updated for BT-CH-0046.
--
-- Revision 1.7  2007/02/15 13:16:02  Niklas
-- Updated for BT-CH-0045.
--
-- Revision 1.6  2006/11/28 19:23:44  niklas
-- Updated for BT-CH-0037.
--
-- Revision 1.5  2005/08/25 20:52:01  niklas
-- Added the option -srec to load more code or data from
-- an S-record file, for example the Lego Mindstorms ROM.
-- Added the option -sym to load and define more symbols
-- from a text file, for example the ROM subprograms.
--
-- Revision 1.4  2005/05/09 15:57:29  niklas
-- Added (fixed) option Move_Signed_Words.
--
-- Revision 1.3  2005/03/22 20:44:05  niklas
-- Added the option Unsigned_Cond, with syntax "-bcc=signed"
-- or "-bcc=unsigned", to support arithmetic analysis.
--
-- Revision 1.2  2005/01/26 20:10:44  niklas
-- Added procedure Finish to cross-check and adjust option values.
--
-- Revision 1.1  2004/06/16 07:41:35  niklas
-- First version.
--


with Options;
with Options.Common;
with Options.String_Sets;


package Decoder.Opt is


   --
   ---   Common options for target-specific processing:
   --
   -- These used to be defined here, but are now defined in
   -- the Options.Common package. For upwards compatibility,
   -- we provide renamings here.


   Trace : Boolean renames Options.Common.Trace_Decode.Value;

   Trace_Effect : Boolean renames Options.Common.Trace_Effect.Value;


   --
   ---   Target-specific options:
   --


   type Signedness_T is (Signed, Unsigned);
   --
   -- Whether a condition is seen as signed or unsigned.


   package Signedness_Valued is new Options.Discrete_Valued (
      Value_Type  => Signedness_T,
      Value_Image => Signedness_T'Image);
   --
   -- Options choosing between signed or unsigned interpretation.


   Cond_Sign_Opt : aliased Signedness_Valued.Option_T (Default => Unsigned);
   --
   -- Whether signed branch conditions are modelled as signed or unsigned.
   -- An unsigned interpretation is valid if the loop counters are in the
   -- non-negative signed range.
   --
   function Unsigned_Cond return Boolean;
   --
   -- Whether signed branch conditions are modelled as unsigned.


   S_Record_File_Opt : aliased Options.String_Sets.Option_T;
   --
   -- The names of optional S-record files to be loaded together
   -- with the main executable target file. These S-record files can
   -- contain some ROM code or data, for example.


   --
   ---   Setting other options
   --


   procedure Handle_Wild_Option (
      Name  : in     String;
      Valid :    out Boolean);
   --
   -- Handles a command-line option of the form "-<Name>", when
   -- the Name is not the name of any registered option.


   procedure Set_No_Deallocation;
   --
   -- Implements the target-specific part of "-imp no_dealloc", the
   -- option that prevents all uses of Unchecked_Deallocation.


   procedure Finish;
   --
   -- Finishes the setting of options, after all command-line option
   -- arguments have been processed, and before reading the executable
   -- target program that will be analyzed.


end Decoder.Opt;
