-- HRT (decl)
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
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: hrt.ads,v $
-- Revision 1.6  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.5  2007-01-25 21:25:17  niklas
-- BT-CH-0043.
--
-- Revision 1.4  2001/04/04 13:26:36  ville
-- Call component added to EntryObject_T
--
-- Revision 1.3  2001/03/20 13:38:55  ville
-- HRT trace flag added
--
-- Revision 1.2  2001/03/19 12:36:23  ville
-- HRT rework third phase WCP converted to ESL lines
--
-- Revision 1.1  2001/03/14 14:51:53  ville
-- HRT rework second phase: skeleton package splitted
--

with Programs;
with Unbounded_Vectors;
with Processor;
with Bags;

package HRT is


   Deallocate : Boolean := True;
   --
   -- Whether to use Unchecked_Deallocation to release unused
   -- heap memory. Declared here to make it usable in instances
   -- of Unbounded_Vectors; meant to be accessed via HRT.Opt.


   type Name_T is access String;
   -- Used for Thread_Name, PO_Name, Entry_Name etc.

   type Name_List_T is array (Positive range <>) of Name_T;

   type String_List_Ref is access Name_List_T;


   type Entry_Object_T;

   type Thread_Object_T;
   

   type Entry_T  is access Entry_Object_T;

   type Thread_T is access Thread_Object_T;

   type Thread_List_T is array (Positive range <>) of Thread_T;

   type Entry_List_T is array (Positive range <>) of Entry_T;

   type Entry_List_Ref is access Entry_List_T;


   type ES_List_T;

   type ES_List_Ref is access ES_List_T;


   type ES_Line_T;

   type ES_T is access ES_Line_T;

   type ES_List_T is array (Positive range <>) of ES_T;


   package ESL_Vectors is new Unbounded_Vectors
     (Element_Type   => ES_T,
      Vector_Type    => ES_List_T,
      Initial_Size   => 10,
      Size_Increment => 10,
      Deallocate     => Deallocate);

   subtype ESL_Vector_T is ESL_Vectors.Unbounded_Vector;

   type ESL_Vector_Ref is access ESL_Vector_T;

   type ES_Kind_T is (Wcet, Call_Po, Loop_Statement);

   type ES_Line_T(Kind : ES_Kind_T) is record
      case Kind is

         when Wcet =>
            Effort : Processor.Time_T;
            -- TBM to Effort with time and number of reads and writes.

         when Call_Po =>
            To : Entry_T;

         when Loop_Statement =>
            Loop_Count : Natural;
            Statements : ESL_Vector_Ref;

      end case;
   end record;

--   type Execution Statement List

--             An HRT "execution statement list" which is a sequence
--             of "wcet" statements and "call_po" statements with
--             a hierarchical structure of "loop" and "end" brackets.
--             This is a central part of the HRT Execution Skeleton.


   package Entry_Vectors is new Unbounded_Vectors
     (Element_Type   => Entry_T,
      Vector_Type    => Entry_List_T,
      Initial_Size   => 5,
      Size_Increment => 10,
      Deallocate     => Deallocate);

   subtype Entry_Vector_T is Entry_Vectors.Unbounded_Vector;


   function Entry_Address
     (Item : Entry_T)
      return Processor.Code_Address_T;

   package Entry_Bags is new Bags
     (Key_Type  => Processor.Code_Address_T,
      Item_Type => Entry_T,
      Key_Of    => Entry_Address,
      "<"       => Processor."<",  -- for Code_Address_T
      "="       => Processor."=",  -- for Code_Address_T
      Count     => Natural);

   subtype Entry_Set_T is
     Entry_Bags.Bag(Duplicate_Keys_Allowed => False);


   function Entry_Address
     (Item : Thread_T)
      return Processor.Code_Address_T;

   package Thread_Bags is new Bags
     (Key_Type  => Processor.Code_Address_T,
      Item_Type => Thread_T,
      Key_Of    => Entry_Address,
      "<"       => Processor."<",  -- for Code_Address_T
      "="       => Processor."=",  -- for Code_Address_T
      Count     => Natural);

   subtype Thread_Set_T is
     Thread_Bags.Bag(Duplicate_Keys_Allowed => False);


   type Def_Object_T;

   type Def_T is access Def_Object_T;


   type Def_Kind_T is (Thread, Resource, Synchro);

   type Thread_Kind_T is (Cyclic, Sporadic, Interrupt_Sporadic);


   type Def_Object_T(Kind : Def_Kind_T) is record

      Comments : String_List_Ref;

      case Kind is
         when Thread =>
            Thread_Name : Name_T;
            Thread_Kind : Thread_Kind_T;
            Root        : Thread_T;

         when Resource =>
            Res_Name    : Name_T;
            PO_Entries  : Entry_Vector_T;

         when Synchro =>
            Sync_Name   : Name_T;
            Po_Entry    : Entry_T;
            Barriered   : Entry_T;
      end case;

   end record;


   type Def_List_T is array (Positive range <>) of Def_T;


   package Def_Vectors is new Unbounded_Vectors
     (Element_Type   => Def_T,
      Vector_Type    => Def_List_T,
      Initial_Size   => 20,
      Size_Increment => 20,
      Deallocate     => Deallocate);


   subtype Def_Vector_T is Def_Vectors.Unbounded_Vector;


   type Entry_Object_T is record
      Name     : Name_T;
      Def      : Def_T;   -- Access to definition containing this entry.
      Sub      : Programs.Subprogram_T;
      Call     : Programs.Call_T;
      ESL      : ES_List_Ref;
      PO_Calls : Entry_List_Ref;
   end record;

   type Thread_Object_T is new Entry_Object_T;


   type HRT_Struct_T is record
      Prog_Name   : Name_T;
      Definitions : Def_Vector_T;
      PO_Entries  : Entry_Set_T;
      Threads     : Thread_Set_T;
      Comments    : String_List_Ref; -- Comments that are outside definitions.
   end record;

   -- Represents the HRT structure of the target program under
   -- analysis, as initially defined in the TPOF, extended with
   -- callee subprograms and WCET results by the tool.
   -- The main attributes are the set of tasks, the set of
   -- protected objects and entries, and the flow-graphs and
   -- other analysis results of the corresponding subprograms,
   -- up to and including the execution statement lists and
   -- the optional protected-object call lists. In other
   -- words, all of the Execution Skeletion File is stored
   -- in the HRT_Structure, before being emitted in the ESF.



   package String_Vectors is new Unbounded_Vectors
     (Element_Type   => Name_T,
      Vector_Type    => Name_List_T,
      Initial_Size   => 5,
      Size_Increment => 5,
      Deallocate     => Deallocate);

   subtype String_Set_T is String_Vectors.Unbounded_Vector;


   function Search (Sub : Programs.Subprogram_T;
                    Of_Set : Entry_Set_T)
                    return Entry_T;
                    
                    
   procedure Erase (Set : in out Entry_Set_T);


   procedure Add (
     To     : in out Entry_Set_T;
     Adding : in     Entry_T);

   procedure Add (
     To     : in out Entry_Set_T;
     Adding : in     Entry_Set_T);
     
   procedure Add (
     To     : in out Thread_Set_T;
     Adding : in     Thread_T);

   function To_List (Item : Entry_Set_T)
                     return Entry_List_T;
                     
   function To_List (Item : Thread_Set_T)
                     return Thread_List_T;


   function Is_Member
     (Item    : Entry_T;
      Of_Set  : Entry_Set_T) return Boolean;
      
   function Is_Member
     (Item    : Programs.Subprogram_T;
      Of_Set  : Entry_Set_T) return Boolean;

   function Is_Member
     (Item    : Programs.Subprogram_T;
      Of_Set  : Thread_Set_T) return Boolean;
      
                                       
   procedure Check_Sanity(HRT : in HRT_Struct_T);


   function Interacting_Subprograms
     (Program : Programs.Program_T;
      HRT     : HRT_Struct_T)
      return Programs.Subprogram_Set_T;
     --         using :
     --             call graph,
     --             HRT structure
     --         giving:
     --             set of subprograms that call a PO entry, directly
     --             or indirectly)
     
   Trace : Boolean := False;
           
end HRT;
