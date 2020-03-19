-- Assertions.Own.Text (body)
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.15 $
-- $Date: 2015/10/24 20:05:45 $
--
-- $Log: assertions-own-text.adb,v $
-- Revision 1.15  2015/10/24 20:05:45  niklas
-- Moved to free licence.
--
-- Revision 1.14  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.13  2009-12-17 14:05:54  niklas
-- BT-CH-0197: Assertions on instruction roles.
--
-- Revision 1.12  2009-11-27 11:28:06  niklas
-- BT-CH-0184: Bit-widths, Word_T, failed modular analysis.
--
-- Revision 1.11  2009/03/24 07:48:34  niklas
-- BT-CH-0166: String_Pool.Item_T for source-file and marker names.
--
-- Revision 1.10  2009/03/20 18:19:28  niklas
-- BT-CH-0164: Assertion context identified by source-line number.
--
-- Revision 1.9  2008/11/03 07:58:12  niklas
-- BT-CH-0155: Ignore assertions on absent subprograms.
--
-- Revision 1.8  2008/09/24 08:38:50  niklas
-- BT-CH-0146: Assertions on "loop starts <bound> times".
-- BT-CH-0146: Loop-repeat assertions set both lower and upper bound.
-- BT-CH-0146: Report locations of contradictory "count" assertions.
-- BT-CH-0146: Contradictory "count" assertions imply infeasibility.
--
-- Revision 1.7  2008/09/19 10:34:13  niklas
-- BT-CH-0144: Assertion "populations" work again, and more.
--
-- Revision 1.6  2008/07/14 19:16:54  niklas
-- BT-CH-0135: Assertions on "instructions".
--
-- Revision 1.5  2007/12/17 13:54:33  niklas
-- BT-CH-0098: Assertions on stack usage and final stack height, etc.
--
-- Revision 1.4  2007/08/03 19:13:45  niklas
-- Extended procedure Put (Feature_T) to use Programs.Index_And_Name.
--
-- Revision 1.3  2006/10/28 19:52:15  niklas
-- BT-CH-0031.
--
-- Revision 1.2  2006/05/29 11:22:32  niklas
-- BT-CH-0023.
--
-- Revision 1.1  2006/05/27 21:26:37  niklas
-- First version for BT-CH-0020.
--


with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Arithmetic;
with Output;
with Processor;
with Storage.Bounds;


package body Assertions.Own.Text is


   use Ada.Strings.Unbounded;


   function Image (Item : Time_Bound_T) return String
   is
      use Storage.Bounds;
      use type Arithmetic.Value_T;
      use type Processor.Time_T;

      Interval : Storage.Bounds.Interval_T;
      -- The interval that corresponds to the given time bound.

   begin

      if Item.Min > 0 then

         Interval.Min := Limit (Arithmetic.Value_T (Item.Min));

      else

         Interval.Min := Not_Limited;

      end if;

      if Item.Max < Processor.Time_T'Last then

         Interval.Max := Limit (Arithmetic.Value_T (Item.Max));

      else

         Interval.Max := Not_Limited;

      end if;

      return Image (Item => Interval, Name => "et");

   end Image;


   function Image (
      Item : Storage.Bounds.Interval_T;
      Name : String)
   return String
   renames Storage.Bounds.Image;


   function Image (Item : Programs.Subprogram_List_T)
   return String
   --
   -- The names of the subprograms, separated by "or".
   --
   is
   begin

      if Item'Length = 0 then

         return "(none)";

      elsif Item'Length = 1 then

         return Programs.Name (Item(Item'First));

      else

         return
              Programs.Name (Item(Item'First))
            & " or "
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Image (Item : Fact_T) return String
   is

      Prefix : constant String :=
           Fact_Kind_T'Image (Item.Kind)
         & " ("
         & Image (Item.Source)
         & ") ";
      -- The common prefix for all Kinds of facts.

   begin

      case Item.Kind is

      when Variable_Invariance =>

         return Prefix & Image (Item.Invariant_Var_Name);

      when Variable_Value =>

         return Prefix
            & Fact_Span_T'Image (Item.Span)
            & ' '
            & Image (
               Item => Item.Var_Value,
               Name => Image (Item.Var_Name));

      when Property_Value =>

         return Prefix
            & Image (
               Item => Item.Prop_Value,
               Name => Processor.Property_T'Image (Item.Property));

      when Execution_Time =>

         return Prefix
            & Image (Item.Time);

      when Stack_Final | Stack_Usage =>

         return Prefix
            & Image (
                 Item => Item.Stack_Value,
                 Name => "stack#" & Output.Image (Item.Stack_Index));

      when Count_Fact_Kind_T =>

         return Prefix
            & Flow.Execution.Image (Item.Count);

      when Instruction_Role =>

         return Prefix
            & Processor.Instruction_Role_T'Image (Item.Role);

      when Return_Method =>

         return Prefix & Calling.Image (Item.Return_Method);

      when Subprogram_Arithmetic =>

         return Prefix & Boolean'Image (Item.Use_Arithmetic);

      when Subprogram_Enough_For_Time =>

         return Prefix & Boolean'Image (Item.Enough_For_Time);

      when Subprogram_Integrate =>

         return Prefix & " integrate calls";

      when Subprogram_Omit =>

         return Prefix & " omit";

      when Subprogram_Unused =>

         return Prefix & " never called";

      when Subprogram_Hide =>

         return Prefix & " hide in call-graph drawing";

      when Call_Callees =>

         return Prefix & Image (Item.Callees.all);

      when Jump_Targets =>

         return Prefix & "TBA";

      end case;

   end Image;


   function Image (Item : Part_Name_T) return String
   is

      Delim : constant String := " [" & Item.Delimiter & ']';
      -- Shows the delimiter.

   begin

      case Item.Kind is

      when Name =>

         return '"' & To_String (Item.Name) & '"' & Delim;

      when Pattern =>

        return "like """ & To_String (Item.Name) & '"' & Delim;

      when Address =>

        return "address """ & To_String (Item.Address) & '"' & Delim;

      end case;

   end Image;


   function Image (Item : Parts_T) return String
   is

      Prefix : constant String :=
           Storage.Bounds.Image (Item.Population)
         & ' '
         & Part_Kind_T'Image (Item.Kind)
         & " (" & Image (Item.Source) & ") ";
      -- The common prefix for all kinds of parts description.

   begin

      case Item.Kind is

      when Program
         | Subprogram
         | Luup
         | Call_Static
         | Call_Dynamic
         | Jump =>

         return Prefix;

      when Reading
         | Writing =>

         return Prefix & Image (Item.Variable);

      when Instruction =>

         return Prefix & " at " & Processor.Image (Item.Address);

      end case;

   end Image;


   procedure Set_Col (Indent : Natural)
   is
      use Ada.Text_IO;
   begin

      Set_Col (Positive_Count (Indent + 1));

   end Set_Col;


   function Image (Item : Source_Position_T) return String
   is

      Suffix : constant String :=
           " within "
         & Image (Item.Fuzz)
         & " in file """
         & Symbols.Image (Item.File)
         & '"';

   begin

     case Item.Kind is

     when Line =>

        return "line" & Line_Number_T'Image (Item.Line_Number) & Suffix;

     when Mark =>

        return "marker """ & To_String (Item.Marker) & '"' & Suffix;

     when Any =>

        return "any point" & Suffix;

     end case;

   end Image;


   procedure Put (
      Item   : Feature_T;
      Indent : Natural;
      Outer  : Goals_T)
   is
      use Ada.Text_IO;
   begin

      Set_Col (Indent);

      if Item.Negated then

         Put ("not ");

      end if;

      Put (Feature_Kind_T'Image (Item.Kind));

      Put (" (" & Image (Item.Source) & ") ");

      case Item.Kind is

      when Subprogram_Identity =>

         Put_Line (Programs.Index_And_Name (Item.Subprogram));

      when Subprogram_Absent | Named =>

         Put_Line (Image (Item.Name));

      when Contains =>

         Put_Line (Image (Item.Parts.all));

         Put (
            Item   => Item.Parts.Predicate,
            Indent => Indent + 3,
            Outer  => Outer);

      when Within =>

         Put_Line (Image (Item.Whole.all));

         Put (
            Item   => Item.Whole.Predicate,
            Indent => Indent + 3,
            Outer  => Outer);

      when Uses | Defines =>

         Put_Line (Image (Item.Variable));

      when Eternal =>

         New_Line;

      when Sourced =>

         Put_Line (
              Source_Relation_T'Image (Item.Source_Rel)
            & ' '
            & Image (Item.Source_Pos));

      when Labelled =>

         Put_Line (
              To_String (Item.Label)
            & " = "
            & Processor.Image (Item.Label_Address));

      when Executes =>

         Put_Line (Processor.Image (Item.Address));

      end case;

   end Put;


   procedure Put (
      Item   : Part_Predicate_Ref;
      Indent : Natural;
      Outer  : Goals_T)
   is
      use Ada.Text_IO;
   begin

      if Item = null then

         Set_Col (Indent);

         Put_Line ("no predicate");

      elsif Is_Member (Item, Outer) then

         Set_Col (Indent);

         Put_Line ("predicate already shown on outer level");

      else

         Set_Col (Indent);

         Put_Line ("Features:" & Natural'Image (Item'Length));

         for I in Item'Range loop

            Put (
               Item   => Item(I),
               Indent => Indent,
               Outer  => Outer & Item);

         end loop;

      end if;

   end Put;


   procedure Put (Item : Assertion_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line ("Assertion (" & Image (Item.Source) & "):");
      Put_Line ("- Fact   : " & Image (Item.Fact));
      Put_Line ("- Context: " & To_String (Item.Context));
      Put_Line ("- Parts  : " & Image (Item.Parts.all));

      Put (Item => Item.Parts.Predicate, Indent => 6, Outer => No_Goals);

   end Put;


   procedure Put (Item : Assertion_Bag_T)
   is
      use Ada.Text_IO;
   begin

      Put_Line (
           "Assertion set contains"
         & Natural'Image (Length (Item))
         & " assertions.");

      for I in First (Item) .. Last (Item) loop

         Put (Output.Image (I) & ": ");

         Put (Element (Item, I));

      end loop;

   end Put;


end Assertions.Own.Text;
