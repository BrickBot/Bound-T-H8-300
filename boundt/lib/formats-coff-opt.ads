-- Formats.COFF.Opt (decl)
--
-- Options for the Formats.COFF package.
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
-- $Revision: 1.6 $
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: formats-coff-opt.ads,v $
-- Revision 1.6  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.5  2015/05/22 06:37:32  niklas
-- Moved to opt.
--
-- Revision 1.4  2013-11-16 19:47:01  niklas
-- BT_CH_0251: 21020/SHARC update: Options and Formats.COFF.
--
-- Revision 1.3  2007-03-06 11:06:30  niklas
-- Added Catenate_File_Names.
--
-- Revision 1.2  2004/05/31 20:22:14  niklas
-- Added an option for the length of a line-number entry, with a
-- choice of 16 or 32 bits for the line number component.
-- Added an option for the expected value of the Magic Number in
-- the COFF file header.
--
-- Revision 1.1  2004/04/24 18:08:34  niklas
-- First Tidorum version, as child of Formats.
--
--
-- <Log: coff-opt.ads>
-- Revision 1.2  2001/06/11 12:28:08  holsti
-- Options for 16/32 bit endianness added.
-- Option for symbol-entry length added.
--
-- Revision 1.1  2000/07/02 18:41:54  holsti
-- First version.
--


with Options;
with Options.Bool;
with Options.Pos;


package Formats.COFF.Opt is


   pragma Elaborate_Body;
   --
   -- To register the options and define the priority ordering
   -- of the option groups.


   Group : constant Options.Group_Name_T := Options.Group ("coff");
   --
   -- All the COFF options.


   Trace_Loading_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- If true, during the loading of a COFF file all elements are immediately
   -- output after being loaded.
   -- This can help understand loading problems.
   --
   Trace_Loading : Boolean renames Trace_Loading_Opt.Value;


   Warn_Storage_Class_Opt : aliased Options.Bool.Option_T (Default => False);
   --
   -- Whether to warn about COFF symbols with a Storage Class that we do
   -- not know how to classify.
   --
   Warn_Storage_Class : Boolean renames Warn_Storage_Class_Opt.Value;


   --
   --    Format and lay-out options
   --
   -- The default values are set to match the COFF files created by
   -- the Analog Devices linker for the ADSP-21020 when running on the
   -- Intel/Windows platform. Other values are described in comments,


   package Endian_Valued is new Options.Discrete_Valued (
      Value_Type  => Endian_T,
      Value_Image => Endian_T'Image,
      Quote_Image => False);

   Endian_Opt : aliased Endian_Valued.Option_T (Default => Little);
   --
   -- The endianness to be assumed for the COFF file (both for short and
   -- long data).
   --
   -- On Intel/Linux, the ADI tool-chain (g21) uses Little.
   -- On SPARC/Unix (Solaris), the ADI tool-chain (g21k) uses Big.
   --
   Endian : Endian_T renames Endian_Opt.Value;


   Short_End : Endian_T := Endian_Opt.Default;
   --
   -- Byte-order of "short" data (16 bits = 8 + 8).


   Long_End  : Endian_T := Endian_Opt.Default;
   --
   -- Word-order of "long" data (32 bits = 16 + 16).
   -- The Short_End order is applied to each 16-bit half.


   Magic_Number : Ushort_T := 0;
   --
   -- The value expected for the File_Header.Magic_Number component.
   -- This identifies the target machine and should always be set
   -- accordingly; the default value is only for determinism.


   Symbol_Entry_Length_Opt : aliased Options.Pos.Option_T (Default => 18);
   --
   -- Number of bytes in a symbol entry.
   -- The data in a symbol entry are padded with trailing zero bytes
   -- up to this length.
   --
   -- For the ADI tool-chain on Unix, use 20.
   --
   Symbol_Entry_Length : Positive renames Symbol_Entry_Length_Opt.Value;


   type Line_Number_Length_T is (Short, Long);
   --
   -- The length of a source-code line-number in the file.
   -- Short means unsigned 16 bits and Long means unsigned 32 bits.


   Line_Number_Length : Line_Number_Length_T := Short;
   --
   -- The length of the line-number component of a source-code
   -- line-number item. The other component of such an item is the
   -- corresponding code address, an unsigned 32-bit quantity.


   Catenate_File_Names : Boolean := True;
   --
   -- Whether the file-name strings from successive Aux_File_Name
   -- symbols belonging to a ".file" symbol should be concatenated
   -- into the full name of the source file.


end Formats.COFF.Opt;
