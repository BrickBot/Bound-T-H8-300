-- Decoder.Trace (body)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder-trace.adb,v $
-- Revision 1.5  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.4  2005/09/03 11:59:17  niklas
-- BT-CH-0006.
--
-- Revision 1.3  2005/01/26 20:12:14  niklas
-- Updated the heading for "Effort" to "Time", since Effort_T is now
-- just the number of execution states = the execution time.
--
-- Revision 1.2  2004/06/27 15:49:11  niklas
-- Added Start and Stop procedures.
--
-- Revision 1.1  2004/06/16 07:41:35  niklas
-- First version.
--


with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Decoder.Opt;
with H8_300.Text;
with Hex;


package body Decoder.Trace is

   use Ada.Text_IO;
   use type Arithmetic.Effect_Ref;
   
   
   -- Columns for the trace-line fields:

   Col_Address   : constant :=  1;
   Col_Hex_Words : constant :=  9;
   Col_Sym_Instr : constant := 20;
   Col_Effect    : constant := 41;
   Col_Effort    : constant := 71;
   Col_Steps     : constant := 79;
   Col_Remark    : constant := 86;


   Steps : Ada.Strings.Unbounded.Unbounded_String;
   --
   -- Accumulates the steps (as step indices).


   Remarks : Ada.Strings.Unbounded.Unbounded_String;
   --
   -- Accumulates the remarks.


   procedure Accumulate (
      To   : in out Ada.Strings.Unbounded.Unbounded_String;
      Text : in     String)
   is
      use Ada.Strings.Unbounded;
   begin

      if Length (To) = 0 then

         To := To_Unbounded_String (Text);

      else

         Append (Source   => To,
                 New_Item => ", ");
         
         Append (Source   => To,
                 New_Item => Text);

      end if;

   end Accumulate;


   procedure Headings
   --
   -- Outputs the headings for the fields of the trace lines.
   --
   is
   begin

      New_Line;
      Set_Col (Col_Address  ); Put ("Address");
      Set_Col (Col_Hex_Words); Put ("Code");

      Set_Col (Col_Sym_Instr);

      if Opt.Trace_Effect then
         Put ("Assembly / Effect");
      else
         Put ("Assembly");
      end if;

      Set_Col (Col_Effort   ); Put ("Time");
      Set_Col (Col_Steps    ); Put ("Steps");
      Set_Col (Col_Remark   ); Put ("Remarks");
      New_Line;

   end Headings;


   procedure Start_Decoding (
      Name     : in String;
      Resuming : in Boolean)
   is
   begin

      New_Line;

      Put ("Decoding ");

      if Resuming then
         Put ("resumes");
      else
         Put ("starts");
      end if;

      Put_Line (" for subprogram " & Name);

      Headings;

   end Start_Decoding;


   procedure Step_Tag (Item : in Flow.Step_Tag_T)
   is
   begin

      Set_Col (Col_Address);

      Put (Flow.Image (Item));

   end Step_Tag;


   procedure Code_Words (
      Address : in Processor.Code_Address_T;
      Code    : in H8_300.Word_List_T)
   is
   begin

      Set_Col (Col_Hex_Words);

      for C in Code'Range loop

         Put (Hex.Image (Hex.Word16_T (Code(C))));

         Put (' ');

      end loop;

   end Code_Words;


   procedure Instruction (Item : in H8_300.Instruction_T)
   is
   begin

      Set_Col (Col_Sym_Instr);

      Put (H8_300.Text.Image (Item));

   end Instruction;


   procedure Result (
      Effect : in Arithmetic.Effect_Ref;
      Effort : in Processor.Effort_T)
   is
   begin

      if Opt.Trace_Effect then

         Set_Col (Col_Effect);
         Put (Arithmetic.Image (Effect.all));

      end if;

      Set_Col (Col_Effort);
      Put (Processor.Image (Effort));

   end Result;


   procedure Step (Item : in Flow.Step_T)
   is
      use Ada.Strings, Ada.Strings.Fixed;
      use Flow;
   begin

      Accumulate (
         To   => Steps,
         Text => Trim (Step_Index_T'Image (Index (Item)), Left));

   end Step;


   procedure Remark (Text : in String)
   is
      use Ada.Strings.Unbounded;
   begin

      Accumulate (Remarks, Text);

   end Remark;


   procedure Done
   is
      use Ada.Strings.Unbounded;
   begin

      if Length (Steps) > 0 then

         Set_Col (Col_Steps);
         Put (To_String (Steps));
         Steps := Null_Unbounded_String;

      end if;

      if Length (Remarks) > 0 then

         Set_Col (Col_Remark);
         Put (To_String (Remarks));
         Remarks := Null_Unbounded_String;

      end if;

      New_Line;

   end Done;


   procedure Stop_Decoding (
      Name      : in String;
      Graph     : in Flow.Graph_T;
      Suspended : in Boolean)
   is
   begin

      New_Line;

      Put ("Decoding ");

      if Suspended then
         Put ("suspended");
      else
         Put ("finished");
      end if;

      Put_Line (
           " for subprogram " & Name & ":"
         & Flow.Step_Index_T'Image (Flow.Max_Step (Graph))
         & " steps,"
         & Flow.Step_Edge_Index_T'Image (Flow.Max_Step_Edge (Graph))
         & " edges.");

      New_Line;

   end Stop_Decoding;


end Decoder.Trace;
