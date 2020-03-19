-- HRT (body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: hrt.adb,v $
-- Revision 1.6  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.5  2009-01-18 08:51:49  niklas
-- Removed unused locals.
--
-- Revision 1.4  2005/02/16 21:11:46  niklas
-- BT-CH-0002.
--
-- Revision 1.3  2002/11/29 11:08:34  holsti
-- Using Programs.Subprograms_Ascending instead of local sort operation.
-- This fixes NC_0146.
--
-- Revision 1.2  2001/03/19 12:36:20  ville
-- HRT rework third phase WCP converted to ESL lines
--
-- Revision 1.1  2001/03/14 14:52:12  ville
-- HRT rework second phase: skeleton package splitted
--

with Output;
with HRT.TPOF;


package body HRT is


   function Entry_Address
     (Item : Entry_T)
      return Processor.Code_Address_T
   is
   begin
      return Programs.Entry_Address(Item.Sub);
   end Entry_Address;


   function Entry_Address
     (Item : Thread_T)
      return Processor.Code_Address_T
   is
   begin
      return Programs.Entry_Address(Item.Sub);
   end Entry_Address;


   function To_List (Item : Entry_Set_T)
                     return Entry_List_T
   is
   begin
      return Entry_List_T (Entry_Bags.To_List (Item));
   end To_List;


   function To_List (Item : Thread_Set_T)
                     return Thread_List_T
   is
   begin
      return Thread_List_T (Thread_Bags.To_List (Item));
   end To_List;


   function Entry_Roots
     (Entries : Entry_Set_T)
      return Programs.Subprogram_Set_T;

   --
   -- Operations on Bags.

   function Is_Member
     (Item    : Entry_T;
      Of_Set  : Entry_Set_T) return Boolean
   is
   begin
      return Entry_Bags.Member
        (Key    => Entry_Address (Item),
         Of_Bag => Of_Set);
   end Is_Member;


   function Is_Member
     (Item    : Programs.Subprogram_T;
      Of_Set  : Entry_Set_T) return Boolean
   is
   begin
      return Entry_Bags.Member
        (Key    => Programs.Entry_Address (Item),
         Of_Bag => Of_Set);
   end Is_Member;


   function Is_Member
     (Item    : Programs.Subprogram_T;
      Of_Set  : Thread_Set_T) return Boolean
   is
   begin
      return Thread_Bags.Member
        (Key    => Programs.Entry_Address (Item),
         Of_Bag => Of_Set);
   end Is_Member;
   

   procedure Add (
     To     : in out Entry_Set_T;
     Adding : in     Entry_T)
   is
   begin
      Entry_Bags.Insert
        (Item => Adding,
         Into => To);

   exception
       when Entry_Bags.Duplicate_Key =>
          -- It is already in the set, OK.
          null;

   end Add;


   procedure Add (
     To     : in out Entry_Set_T;
     Adding : in     Entry_Set_T)
   is

      Add_List : constant Entry_Bags.List :=
        Entry_Bags.To_List (Adding);

   begin
      for A in Add_List'range loop
         Add (To => To, Adding => Add_List(A));
      end loop;

   end Add;


   function Search (Sub : Programs.Subprogram_T;
                    Of_Set : Entry_Set_T)
                    return Entry_T
   is
      Item : Entry_T;
   begin
      Item := Entry_Bags.Search(Programs.Entry_Address(Sub), Of_Set);
      return Item;
   exception
      when Entry_Bags.Nonexistent_Key =>
         return null;
   end Search;



   procedure Add (
     To     : in out Thread_Set_T;
     Adding : in     Thread_T)
   is
   begin
      Thread_Bags.Insert
        (Item => Adding,
         Into => To);

   exception
       when Thread_Bags.Duplicate_Key =>
          -- It is already in the set, OK.
          null;

   end Add;


   function Search (Sub : Programs.Subprogram_T;
                    Of_Set : Thread_Set_T)
   return Thread_T
   --
   -- Searches for a thread that has the root subprogram Sub.
   --
   is
      Item : Thread_T;
   begin
      Item := Thread_Bags.Search(Programs.Entry_Address(Sub), Of_Set);
      return Item;
   exception
      when Thread_Bags.Nonexistent_Key =>
         return null;
   end Search;


   procedure Erase (Set : in out Entry_Set_T)
   is
   begin
      Entry_Bags.Destroy(Set);
   end Erase;


   function Interacting_Subprograms (
      Program : Programs.Program_T;
      HRT     : HRT_Struct_T)
   return Programs.Subprogram_Set_T
   --
   -- The set of "interacting" subprograms, that is those subprograms
   -- that call a PO entry, directly or indirectly.
   --
   is

      Subs_Ascending : constant Programs.Subprogram_List_T :=
         Programs.Subprograms_Ascending (Program);
      -- All subprograms in bottom-up calling order.

      PO_Entries : constant Programs.Subprogram_Set_T :=
         Entry_Roots (HRT.PO_Entries);
      -- All PO entries.

      Interacting : Programs.Subprogram_Set_T;
      -- Collects the subprograms that interact with PO entries.


      procedure Inspect (Caller : in Programs.Subprogram_T)
      --
      -- Checks if a given subprogram interacts directly or
      -- indirectly with PO entries and if so, adds it to the
      -- Interacting set.
      --
      is

         Calls : constant Programs.Call_List_T :=
            Programs.Calls_From (Caller);
         -- All the calls from Caller.

         Callee : Programs.Subprogram_T;
         -- A subprogram called from Caller.

      begin

         for C in Calls'Range loop

            Callee := Programs.Callee (Calls(C));

            if Programs.Is_Member (Callee, PO_Entries)
            or Programs.Is_Member (Callee, Interacting)
            then
               -- This subprogram calls a PO entry directly, or
               -- calls another subprogram already known to be
               -- interacting with PO entries. Therefore this
               -- subprogram also interacts with PO entries.

               Programs.Add (
                  To     => Interacting,
                  Adding => Caller);

               exit;
                  
            end if;

         end loop;

      end Inspect;


   begin  -- Interacting_Subprograms

      -- Initialise the set of interacting subprograms to empty:

      Programs.Erase (Interacting);

      for S in Subs_Ascending'Range loop

         Inspect (Caller => Subs_Ascending(S));

      end loop;

      -- Return the set of interacting subprograms:

      return Interacting;

   end Interacting_Subprograms;


   procedure Check_Sanity(HRT : in HRT_Struct_T)
      --
      -- if some thread root subprogram is called by
      --    some other subprogram (thread or PO entry)
      -- then
      --    Signal an error (contradiction in HRT structure)
      --    and abort analysis;
      -- end if;

      -- Possibly other sanity checks (to be defined) of calling
      -- relationships between PO entries and threads;

   is
      
      PO_Entries : Entry_List_T  := To_List(HRT.PO_Entries);
      Threads    : Thread_List_T := To_List(HRT.Threads);
      Errors     : Natural := 0;

   begin

         for T in Threads'range loop
            declare
               Calls : Programs.Call_List_T :=
                 Programs.Calls_From (Threads(T).Sub);
            begin
               for C in Calls'Range loop
                  if Is_Member(Programs.Callee(Calls(C)), HRT.Threads) then
                     declare
                        Thread : Thread_T :=
                          Search(Programs.Callee(Calls(C)), HRT.Threads);
                     begin
                        Errors := Errors + 1;
                        Output.Error(Text => "Thread "
                                     & Threads(T).Name.all
                                     & " calls another thread "
                                     & Thread.Name.all & ".");
                     end;
                  end if;
               end loop;
            end;
         end loop;

         for E in PO_Entries'Range loop
            declare
               Calls : Programs.Call_List_T :=
                 Programs.Calls_From (PO_Entries(E).Sub);
            begin
               for C in Calls'Range loop
                  if Is_Member(Programs.Callee(Calls(C)), HRT.Threads) then
                     declare
                        Thread : Thread_T :=
                          Search(Programs.Callee(Calls(C)), HRT.Threads);
                     begin
                        Errors := Errors + 1;
                        Output.Error(Text => "Protected object "
                                     & PO_Entries(E).Name.all
                                     & " calls a thread "
                                     & Thread.Name.all & ".");
                     end;

                  end if;

               end loop;
            end;

         end loop;

         if Errors > 0 then
            Output.Error(Text => "HRT: " & Integer'Image(Errors) &
                         " contradictions in HRT structure.");
            raise TPOF.TPOF_Error;
         end if;

   end Check_Sanity;


   function Entry_Roots
     (Entries : Entry_Set_T)
      return Programs.Subprogram_Set_T
   is
      --
      -- Return entries as a set of subprograms.

      Sub_Set : Programs.Subprogram_Set_T;
      E_List  : Entry_List_T :=  To_List(Entries);

   begin
      Programs.Erase(Sub_Set);

      for E in E_List'Range loop
         Programs.Add(To     => Sub_Set,
                      Adding => E_List(E).Sub);
      end loop;

      return Sub_Set;

   end Entry_Roots;


end HRT;
