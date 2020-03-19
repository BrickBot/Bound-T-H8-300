-- Symbols.Show (body)
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
-- $Date: 2015/10/24 20:05:52 $
--
-- $Log: symbols-show.adb,v $
-- Revision 1.6  2015/10/24 20:05:52  niklas
-- Moved to free licence.
--
-- Revision 1.5  2005-10-09 08:10:24  niklas
-- BT-CH-0013.
--
-- Revision 1.4  2005/03/25 17:52:59  niklas
-- Removed exception handling and the exception message from the
-- Statement function because it could cause a vicious recursion
-- cycle when Output.Exception_Info tries to find the locus for the
-- exception message.
--
-- Revision 1.3  2004/04/25 11:54:12  niklas
-- First Tidorum version.
-- Provides Statement_Locus_T for source-line connections.
--
-- Revision 1.2  2001/03/26 13:56:28  holsti
-- Locus for connection added.
--
-- Revision 1.1  2001/03/21 20:31:25  holsti
-- First version.
--
--


package body Symbols.Show is


   function Statement (
      Line   : Connection_T;
      Source : Symbol_Table_T)
   return Output.Statement_Locus_T
   is

      Scope : constant Scope_T := Scope_Of (Line);
      -- The scope of a line-number connection should have
      -- at least two levels: source-file, subprogram.

      Line_Number : Output.Line_Number_T := Line_Number_Of (Line);
      -- The source-line number connected.

   begin

      if Depth (Scope) >= 1 then
         -- At least we have a source-file name.

         return
            Output.Locus (
               Source_File  => Name_Of (Level => 1, Within => Scope),
               Line_Number  => Line_Number,
               Code_Address => Address_Of (Line),
               Symbol_Table => Source);

      else
         -- No source file! Gaah!

         -- We cannot emit a Note, Error, Warning or Fault because this
         -- could create a vicious recursion cycle when Output tries
         -- to find the locus for this Note, Error, Warning or Fault.
         -- (We could do that if we could supply a fixed, fully defined
         -- locus that would be guaranteed to need no further look-up.)

         return
            Output.Locus (
               Source_File  => "",
               Line_Number  => Line_Number,
               Code_Address => Address_Of (Line),
               Symbol_Table => Source);

      end if;

   end Statement;


   function Statements (
      Address : Processor.Code_Address_T;
      Source  : Symbol_Table_T)
   return Output.Statement_Range_T
   is
      use type Output.Statement_Range_T;

      Stats : Output.Statement_Range_T := Output.No_Statements;
      -- 
      -- The statements to be returned.
 
      Connections : constant Connection_Set_T :=
         Connections_For_Address (
            Address => Address,
            Within  => Source);
      --
      -- Connections for the given address.

   begin

      for C in Connections'Range loop
      
         if Kind_Of (Connections(C)) = Line_Number then

            Stats :=
                 Stats
               + Statement (Line => Connections(C), Source => Source);

         end if;
         
      end loop;

      if Stats = Output.No_Statements then
         -- No line-number connnections found.
         -- At least the code address is known.

         Stats :=
            + Output.Locus (
                 Code_Address => Address,
                 Symbol_Table => Source);

      end if;
      
      return Stats;
      
   end Statements;


   function Locus (
      Connection : Connection_T;
      Source     : Symbol_Table_T)
   return Output.Locus_T
   is
      use type Output.Statement_Range_T;

   begin

      case Kind_Of (Connection) is

         when Subprogram
            | Label      =>

            return Output.Locus (
               Source_File => Image (Scope_Of (Connection)),
               Call_Path   => Name_Of (Connection),
               Statements  =>
                  Statements (
                     Address => Address_Of (Connection),
                     Source  => Source));

         when Line_Number =>

            return Output.Locus (
               Statements  => + Statement (Connection, Source));

         when Cell =>

            Output.Fault (
               Location => "Symbols.Show.Locus",
               Text => "Connection refers to a cell.");

            return Output.No_Locus;

      end case;

   end Locus;


end Symbols.Show;
