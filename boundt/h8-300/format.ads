-- Format (decl)
--
-- Executable format loader for H8/300 executable programs.
-- Provides the program decoder with access to the instructions,
-- constant data, and symbol tables in the program to be analysed.
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
-- $Revision: 1.7 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: format.ads,v $
-- Revision 1.7  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.6  2007/12/27 23:12:33  niklas
-- BT-CH-0104: S-record program format for Bound-T/H8-300.
--
-- Revision 1.5  2005/08/25 21:07:52  niklas
-- Added Load_S_Record_File.
--
-- Revision 1.4  2005/05/09 16:06:35  niklas
-- Added a Form attribute to Object_T. To support this, moved the types
-- Form_T and Known_Form_T here, from Format.Opt. The Decoder uses the
-- Form attribute to choose between GCC or IAR calling protocols.
--
-- Revision 1.3  2005/04/01 14:21:21  niklas
-- Added the Word function.
-- Made Object.Content aliased to support UBROF.
--
-- Revision 1.2  2005/03/17 07:22:25  niklas
-- Adapted to privatization of Program_T attributes as follows.
-- Changed Object_T to be a record type instead of an access.
-- The access (reference) level is now provided by the type
-- Processor.Program.Info_T. File_Object_T became redundant.
-- Changed Load_File.Object to be "access" mode, to allow the
-- direct use of the query function Programs.Processor_Info.
-- Changed Load_File.Symbol_Table to be "in" mode as befits a
-- reference type.
--
-- Revision 1.1  2004/06/16 07:41:37  niklas
-- First version.
--


with H8_300;
with Memory;
with Processor;
with Symbols;


package Format is


   --
   --    Loading code and data into a memory-image object.
   --


   type Form_T is (
      COFF,
      UBROF,
      SRec,
      Unknown);
   --
   -- The available options for executable file format:
   --
   -- COFF
   --    Base Format: Common Object File Format
   --    Extensions : TBD
   --    Compiler   : GNU H8 cross-compiler.
   --
   -- UBROF
   --    Universal Binary Relocatable Object Format, a proprietary format
   --    defined by IAR Systems AB.
   --
   -- SRec
   --    S-records, a textual format for binary memory images originally (?)
   --    defined by Motorola. No symbolic debugging information.
   --
   -- Unknown
   --    Format to be determined (guessed) by inspecting the file.
   --    Must be last in the list.


   subtype Known_Form_T is Form_T range
      Form_T'First .. Form_T'Pred (Unknown);
   --
   -- The known and recognized formats.


   type Object_T is limited private;
   --
   -- Represents the contents of an executable file.


   function Form (Object : Object_T) return Known_Form_T;
   --
   -- The file format from which the Object was loaded.


   Loading_Error : exception;
   --
   -- Raised after any unrecoverable error in opening and loading an
   -- executable target program file.
   --
   -- The error may be external (e.g. file does not exist) or internal
   -- (e.g. the file is corrupted or in an unsupported form).


   procedure Load_File (
      File_Name    : in     String;
      Object       : access Object_T;
      Symbol_Table : in     Symbols.Symbol_Table_T);
   --
   -- Opens the executable file identified by the given file-name and
   -- returns an Object that provides access to the data in the file
   -- and a symbol-table holding the symbolic (debug) information.
   --
   -- Propagates Loading_Error upon any (unrecoverable) problem, after
   -- reporting the error (using Output).
   --
   -- File_Name
   --    The name of the file to be read and loaded.
   -- Object
   --    Created to provide access to the contents of the file.
   -- Symbol_Table
   --    Extended with the symbolic information loaded from the file.
   --    Note that this is a reference type.


   procedure Load_S_Record_File (
      File_Name : in     String;
      Object    : access Object_T);
   --
   -- Opens the S-record file identified by the given file-name and
   -- loads its contents into the Object's memory image.
   --
   -- Propagates Loading_Error upon any (unrecoverable) problem,
   -- after reporting the errors (using Output).
   --
   -- This operation is intended for loading "auxiliary" S-record files
   -- in addition to the main program file that is loaded with the
   -- above procedure Load_File.
   --
   -- File_Name
   --    The name of the S-record file to be read and loaded.
   -- Object
   --    The memory-image object (loaded executable file) into
   --    which the S-record code/data will be placed. This object
   --    must already exist.


   procedure Dump_File (File_Name : in  String);
   --
   -- Opens and reads the executable file identified by the given
   -- file-name and describes its contents on standard output.
   --
   -- Does not propagate Loading_Error, whatever happens, but
   -- reports errors via Output.


   --
   --    Reading code and data from a memory-image object.
   --


   Blank_Address : exception;
   --
   -- Raised upon an attempt to read code or data from a loaded executable
   -- image when no code or data has been loaded into this address (because
   -- the executable file did not provide any).


   function Octet (
      Address : Processor.Address_T;
      From    : Object_T)
   return H8_300.Octet_T;
   --
   -- Returns the byte (code or data) at the given address from the
   -- given executable file (which must have been previously loaded
   -- with Load_File).
   --
   -- Propagates Blank_Address if nothing has been loaded into this
   -- address.


   function Word (
      Address : Processor.Address_T;
      From    : Object_T)
   return H8_300.Unsigned_Word_T;
   --
   -- Returns the word (code or data) at the given address from
   -- given executable file (which must have been previously loaded
   -- with Load_File).
   --
   -- Propagates Blank_Address if nothing has been loaded into this
   -- address.


   function Code (
      Address : Processor.Code_Address_T;
      From    : Object_T)
   return H8_300.Unsigned_Word_T;
   --
   -- Returns the H8/300 instruction word at the given address from
   -- given executable file (which must have been previously loaded
   -- with Load_File).
   --
   -- If the instruction needs a second 16-bit word, that word must
   -- be retrieved with another call of Code. Of course, it is also
   -- possible to retrieve a second (ie. the next) word even if the
   -- instruction is a one-word instruction.
   --
   -- Propagates Blank_Address if nothing has been loaded into this
   -- address.


   procedure Fetch_Code (
      From    : in     Object_T;
      Address : in     Processor.Code_Address_T;
      Into    :    out H8_300.Word_List_T;
      Last    :    out Integer);
   --
   -- Reads H8/300 instruction words starting at the given Address,
   -- Into an array of such words. The operation tries to read
   -- Into'Length words, but may actually read fewer words if the
   -- loaded area ends first. The Last parameter shows the last valid
   -- word index, so the valid results are Into(Into'First .. Last).
   -- If no words could be read, Last is returned as Into'First - 1.


private


   type Object_T is limited record
      Form    : Known_Form_T;
      Content : aliased Memory.Content_T;
   end record;
   --
   -- The program-data are stored as a memory content object.
   --
   -- Form
   --    The form of the executable file.
   -- Content
   --    The memory contents as loaded from the file.
   --
   -- TBA e.g. locations of _exit() and other special routines.


end Format;
