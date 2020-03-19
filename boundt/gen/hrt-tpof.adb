-- HRT.TPOF (body)
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: hrt-tpof.adb,v $
-- Revision 1.8  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.7  2007-08-14 12:36:38  niklas
-- BT-CH-0072: Corrections to handling ambiguous names.
--
-- Revision 1.6  2001/06/19 21:41:01  holsti
-- TPO file-name suffix is now ".tpo".
--
-- Revision 1.5  2001/04/12 13:22:02  ville
-- NC_052 fixed
--
-- Revision 1.4  2001/04/11 13:36:03  ville
-- Parameter Delimiter added to Programs.Identify
--
-- Revision 1.3  2001/04/04 13:27:12  ville
-- Programs.Add_Root used
--
-- Revision 1.2  2001/04/03 10:34:09  ville
-- Missing assigment of thread definition added
--
-- Revision 1.1  2001/03/14 14:51:31  ville
-- HRT rework second phase: skeleton package splitted
--


with Ada.Strings.Fixed;
with Ada.Text_IO;

with OpenToken.Text_Feeder.Text_IO;

with Output;
with HRT.Lexer;
with Symbols;

package body HRT.TPOF is


   TPOF_Suffix : constant String := "tpo";
   --
   -- Default and expected suffix of TPOF name.


   ESF_Suffix : constant String := "esf";
   --
   -- Generated suffix for ESF name.


   function Check_TPOF_Name (TPOF : String)
   return String
   --
   -- Checks that TPOF name exists and has the correct suffix.
   -- Returns a string to be used as ESF name.
   --
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Ind : Natural;
      -- The index of the last period in TPOF, or zero
      -- if there is no period.

   begin

      Ind := Index(TPOF, ".", Backward);

      if Ind > 0
      and then TPOF(Ind+1..TPOF'Last) = TPOF_Suffix
      then
         -- The expected suffix is there.
         -- Replace if with the ESF suffix:

         return TPOF(TPOF'First..Ind) & ESF_Suffix;

      else
         -- The expected suffix is not there.

         Output.Warning (Text =>
              "TPOF name does not end with '"
            & TPOF_Suffix & ''');

         -- raise TPOF_Error; TBM ?

         -- Add the ESF suffix:

         return TPOF & '.' & ESF_Suffix;

      end if;

   end Check_TPOF_Name;


   procedure Get_TPOF
     (TPOF_Name  : in     String;
      Program    : in out Programs.Program_T;
      HRT_Struct :    out HRT_Struct_T)
     --    using :
     --        name of TPO file,
     --        access handles to target program
     --    giving:
     --        HRT structure as defined in TPOF,
     --        or failure (errors in TPOF))
   is

      Program_Locus : constant String := Output.Program_File(
         Output.Current_Locus);
         
      use HRT.Lexer;

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize(Syntax);

      File     : Ada.Text_IO.File_Type;

      Old_File : constant Ada.Text_IO.File_Access :=
        Ada.Text_IO.Current_Input;
      -- Store current input file.

      Errors   : Natural := 0;

      All_Found : Boolean := True;
      -- All root subprograms (threads and entries) are found from program.

      Prog_Comments : String_Set_T;

      Roots     : Programs.Subprogram_Set_T;
      -- Collect root subprograms into set.

      function Token_Image return String
      is
      begin
         case Tokenizer.ID(Analyzer) is
            when Barrier_Tok    =>
               return "barrier";
            when End_Of_Text    =>
               return "";
            when End_Tok        =>
               return "end";
            when Entry_Tok      =>
               return "entry";
            when Program_Tok    =>
               return "program";
            when Protected_Tok  =>
               return "protected";
            when Resource_Tok   =>
               return "resource";
            when Root_Tok       =>
               return "root";
            when Thread_Tok     =>
               return "thread";
            when Type_Tok       =>
               return "type";
            when Cyclic_Tok     =>
               return "cyclic";
            when Sporadic_Tok   =>
               return "sporadic";
            when Interrupt_Tok  =>
               return "interrupt_sporadic";
            when Synchro_Tok    =>
               return "synchro";
            when Comment_Tok    =>
               return Tokenizer.Lexeme (Analyzer);
            when Whitespace     =>
               return "";
            when Name_Tok       =>
               return '"' & Tokenizer.Lexeme (Analyzer) & '"';
         end case;
      end Token_Image;

      procedure Output_Error(Msg : in String; Locus : in Output.Locus_T)
      is
         -- Outputs an error message.
         
         use type Output.Locus_T;
         
         Statement_Locus : constant Output.Statement_Locus_T :=
           Output.Locus(Line_Number => Output.Line_Number_T(
              Tokenizer.Line (Analyzer)));
         
      begin
      
         Output.Error(
            Text => "col" &
               Integer'Image(Tokenizer.Column (Analyzer)) &
               " : " & Msg & " Token: " & Token_Image,
            Locus => Locus & Output.Locus(Statements => 
               Output.Rainge(Statement_Locus, Statement_Locus)));
         Errors := Errors + 1;
      end Output_Error;


      procedure Output_Warning(Msg : in String; Locus : in Output.Locus_T)
      is
         -- Outputs a warning message.
         
         use type Output.Locus_T;
         
         Statement_Locus : constant Output.Statement_Locus_T :=
           Output.Locus(Line_Number => Output.Line_Number_T(
              Tokenizer.Line (Analyzer)));

      begin
         Output.Warning(
            Text => "col" &
               Integer'Image(Tokenizer.Column (Analyzer)) &
               " : " & Msg & " Token: " & Token_Image,
            Locus => Locus & Output.Locus(Statements => 
               Output.Rainge(Statement_Locus, Statement_Locus)));
      end Output_Warning;


      procedure Next_Token (Comments : in out String_Set_T)
      is
         -- Finds the next token and checks that there is one.
      begin
         loop
            Tokenizer.Find_Next (Analyzer);
            exit when Tokenizer.ID(Analyzer) /= Comment_Tok;
            String_Vectors.Append(Comments, new String'
                                  (Tokenizer.Lexeme(Analyzer)));
         end loop;

         if Tokenizer.ID(Analyzer) = End_Of_Text then
            Output_Error(
               Msg   => "Unexpected end of file.",
               Locus => Output.Locus (
                 Program_File => Program_Locus,
                 Source_File  => TPOF_Name));
            raise TPOF_Syntax_Error;
         end if;

      end Next_Token;


      function Skip_Till_End
        return String
        --
        -- Skips over the rest of an definition.
        -- Returns the name after keyword end;
      is
         Comments : String_Set_T;
         -- Since this function is called only in an error situation
         -- the comments need not to be returned.

      begin
         loop
            case Tokenizer.ID(Analyzer) is
               when End_Tok =>
                  Next_Token(Comments);
                  return Tokenizer.Lexeme(Analyzer);
               when Others =>
                  Output_Warning(
                     Msg   => "Skipping token.",
                     Locus => Output.Locus (
                        Program_File => Program_Locus,
                        Source_File  => TPOF_Name));
                  Next_Token(Comments);
            end case;
         end loop;
      end Skip_Till_End;

      procedure Find (
         Name  : in     String;
         Msg   : in     String;
         Found :    out Boolean;
         Sub   :    out Programs.Subprogram_T)
      is
      begin

         Programs.Identify (
            Identifier => Name,
            Delimiter  => Symbols.Default_Delimiter,
            Program    => Program,
            Subprogram => Sub);

         Found := True;

      exception

      when Symbols.Ambiguous_Name =>

         Output_Error (
            Msg   => Msg & " " & Name & " is an ambiguous name. ",
            Locus => Output.Locus (
               Program_File => Program_Locus,
               Source_File  => TPOF_Name));

         Found := False;

      when Programs.Subprogram_Not_Found =>

         Output_Error (
            Msg   => Msg & " " & Name & " is not found. ",
            Locus => Output.Locus (
               Program_File => Program_Locus,
               Source_File  => TPOF_Name));

         Found := False;

      end Find;


      function Get_Thread return Def_T
      is
         -- use Thread_Vectors;

         Ret      : Def_T := new Def_Object_T(Thread);
         Comments : String_Set_T;
         Found    : Boolean;

      begin
         Next_Token(Comments);
         Ret.Thread_Name := new String'(Tokenizer.Lexeme(Analyzer));
         Ret.Root := new Thread_Object_T;
         Ret.Root.Def := Ret;

         Next_Token(Comments);
         if Tokenizer.ID(Analyzer) = Type_Tok then
            Next_Token(Comments);
            if Tokenizer.ID(Analyzer) in Thread_Type_T then
               case Tokenizer.ID(Analyzer) is
                  when Cyclic_Tok =>
                     Ret.Thread_Kind := Cyclic;
                  when Sporadic_Tok =>
                     Ret.Thread_Kind := Sporadic;
                  when Interrupt_Tok =>
                     Ret.Thread_Kind := Interrupt_Sporadic;
                  when others =>
                     -- Not possible.
                     null;
               end case;
               Next_Token(Comments);

               if Tokenizer.ID(Analyzer) = Root_Tok then
                  Next_Token(Comments);
                  Ret.Root.Name := new String'(Tokenizer.Lexeme(Analyzer));
                  Next_Token(Comments);
               else
                  Ret.Root.Name := Ret.Thread_Name;
               end if;

               if Tokenizer.ID(Analyzer) = End_Tok then
                  Next_Token(Comments);

                  if Ret.Thread_Name.all /= Tokenizer.Lexeme(Analyzer) then
                     Output_Warning(
                        Msg   => "Thread_Name at end does not match.",
                        Locus => Output.Locus (
                           Program_File => Program_Locus,
                           Source_File  => TPOF_Name,
                           Call_Path    => Ret.Thread_Name.all));
                  end if;

                  -- Find thread from program.
                  Find(Name  => Ret.Root.Name.all,
                       Msg   => "Root subprogram",
                       Found => Found,
                       Sub   => Ret.Root.Sub);

                  if not Found then
                     All_Found := false;
                     Next_Token(Prog_Comments);
                     return null;
                  end if;

                  if not Programs.Is_Member(
                     Subprogram => Ret.Root.Sub,
                     Of_Set     => Roots)
                  then
                     Programs.Add(To     => Roots,
                                  Adding => Ret.Root.Sub);
                                  
                     Programs.Add_Root(
                        Root => Ret.Root.Sub,
                        To   => Program,
                        Call => Ret.Root.Call);
                  else
                     Output_Error (
                        Msg   => "Duplicate entry",
                        Locus => Output.Locus (
                           Program_File => Program_Locus,
                           Source_File  => TPOF_Name,
                           Call_Path    => Ret.Thread_Name.all));
                     raise TPOF_Error;
                  end if;

                  -- Append to HRT object.
                  Add(To     => HRT_Struct.Threads,
                      Adding => Ret.Root);

                  if String_Vectors.Length(Comments) > 0 then
                     -- Add comments, if any.
                     Ret.Comments := new Name_List_T'
                       (String_Vectors.To_Vector(Comments));

                  end if;

                  Next_Token(Prog_Comments);

                  return Ret;
               else
                  Output_Error(
                     Msg   => "End " & Ret.Thread_Name.all & " expected.",
                     Locus => Output.Locus(
                        Program_File => Program_Locus,
                        Source_File  => TPOF_Name,
                        Call_Path    => Ret.Thread_Name.all));
               end if;
            else
               Output_Error(
                  Msg   => "Thread_Type expected.",
                  Locus => Output.Locus(
                     Program_File => Program_Locus,
                     Source_File  => TPOF_Name,
                     Call_Path    => Ret.Thread_Name.all));
            end if;
         else
            Output_Error(
               Msg   => "Type expected.",
                  Locus => Output.Locus(
                     Program_File => Program_Locus,
                     Source_File  => TPOF_Name,
                     Call_Path    => Ret.Thread_Name.all));
         end if;

         -- Skip this protected object
         if Ret.Thread_Name.all /= Skip_Till_End then
            Output_Warning(
               Msg   => "Thread_Name at end does not match.",
               Locus => Output.Locus (
                  Program_File => Program_Locus,
                  Source_File  => TPOF_Name,
                  Call_Path    => Ret.Thread_Name.all));
         end if;

         Next_Token(Prog_Comments);
         return null;

      end Get_Thread;


      function Get_Resource_Or_Synchro return Def_T
      is
         use Entry_Vectors;

         Ret        : Def_T;
         Comments   : String_Set_T;
         Found      : Boolean;
         Both_Found : Boolean := True; -- Both synchro entries found.
      begin
         Next_Token(Comments);

         declare
            Name  : Name_T := new String'(Tokenizer.Lexeme(Analyzer));

         begin
            Next_Token(Comments);

            if Tokenizer.ID(Analyzer) /= Type_Tok then
               Output_Error(
                  Msg   => "Protected object type expected.",
                  Locus => Output.Locus(
                     Program_File => Program_Locus,
                     Source_File  => TPOF_Name,
                     Call_Path    => Name.all));
                  
               -- Skip this protected object
               if Name.all /= Skip_Till_End then
                  Output_Warning(
                     Msg   => "PO_Name at end does not match.",
                     Locus => Output.Locus (
                        Program_File => Program_Locus,
                        Source_File  => TPOF_Name,
                        Call_Path    => Name.all));
               end if;
            else
               Next_Token(Comments);

               case Tokenizer.ID(Analyzer) is
                  when Resource_Tok =>
                     Ret := new Def_Object_T(Resource);
                     Next_Token(Comments);

                     loop
                        if Tokenizer.ID(Analyzer) /= Entry_Tok then
                           Output_Error(
                              Msg  => "PO_Entry expected.",
                              Locus => Output.Locus(
                                 Program_File => Program_Locus,
                                 Source_File  => TPOF_Name,
                                 Call_Path    => Name.all));
                              
                           Next_Token(Comments);

                        else
                           Next_Token(Comments);

                           declare
                              PO_Entry   : Entry_T := new Entry_Object_T;
                              Entry_Name : Name_T := new String'
                                (Tokenizer.Lexeme(Analyzer));
                           begin
                              PO_Entry.Name := Entry_Name;
                              PO_Entry.Def  := Ret;

                              Find(Name  => PO_Entry.Name.all,
                                   Msg   => "Entry subprogram",
                                   Found => Found,
                                   Sub   => PO_Entry.Sub);

                              if not Found then
                                 All_Found := false;
                              else

                                 if not Programs.Is_Member(
                                    Subprogram => PO_Entry.Sub,
                                    Of_Set     => Roots)
                                 then
                                    Programs.Add(To     => Roots,
                                                 Adding => PO_Entry.Sub);
                                                 
                                    Programs.Add_Root(
                                       Root => PO_Entry.Sub,
                                       To   => Program,
                                       Call => PO_Entry.Call);
                                 else
                                    Output_Error (
                                       Msg    => "Duplicate entry " & 
                                          PO_Entry.Name.all,
                                       Locus  => Output.Locus (
                                          Program_File => Program_Locus,
                                          Source_File  => TPOF_Name,
                                          Call_Path    => Name.all));
                                    raise TPOF_Error;
                                 end if;

                                 -- Add PO_Entry to this resource and
                                 -- to PO_Entries on HRT object.
                                 Append(To     => Ret.PO_Entries,
                                        Value  => PO_Entry);
                                 Add(To     => HRT_Struct.PO_Entries,
                                     Adding => PO_Entry);

                              end if;

                              Next_Token(Comments);
                           end;

                        end if;

                        exit when Tokenizer.ID(Analyzer) = End_Tok;

                     end loop;

                     Ret.Res_Name := Name;

                     Next_Token(Comments);

                     if Name.all /= Tokenizer.Lexeme(Analyzer) then
                        Output_Warning(
                           Msg   => "PO_Name at end does not match.",
                           Locus => Output.Locus (
                              Program_File => Program_Locus,
                              Source_File  => TPOF_Name,
                              Call_Path    => Name.all));
                     end if;

                     if String_Vectors.Length(Comments) > 0 then
                        -- Add comments, if any.
                        Ret.Comments := new Name_List_T'
                          (String_Vectors.To_Vector(Comments));

                     end if;

                     Next_Token(Prog_Comments);

                     return Ret;

                  when Synchro_Tok =>

                     Ret := new Def_Object_T(Synchro);
                     Ret.PO_Entry  := new Entry_Object_T;
                     Ret.Barriered := new Entry_Object_T;
                     Ret.PO_Entry.Def  := Ret;
                     Ret.Barriered.Def := Ret;

                     Next_Token(Comments);

                     if Tokenizer.ID(Analyzer) = Entry_Tok then
                        Next_Token(Comments);
                        Ret.PO_Entry.Name := new
                          String'(Tokenizer.Lexeme(Analyzer));
                        Next_Token(Comments);

                        if Tokenizer.ID(Analyzer) = Barrier_Tok then
                           Next_Token(Comments);
                           if Tokenizer.ID(Analyzer) = Entry_Tok then
                              Next_Token(Comments);

                              Ret.Barriered.Name := new
                                String'(Tokenizer.Lexeme(Analyzer));
                              Next_Token(Comments);

                              if Tokenizer.ID(Analyzer) = End_Tok then
                                 Next_Token(Comments);

                                 if Name.all /= Tokenizer.Lexeme(Analyzer) then
                                    Output_Warning(
                                       Msg => "PO_Name at end does not match.",
                                       Locus => Output.Locus (
                                          Program_File => Program_Locus,
                                          Source_File  => TPOF_Name,
                                          Call_Path    => Name.all));
                                 end if;

                                 Ret.Sync_Name := Name;

                                 Find(Name  => Ret.PO_Entry.Name.all,
                                      Msg   => "Entry subprogram",
                                      Found => Found,
                                      Sub   => Ret.PO_Entry.Sub);

                                 if Found then                                 
                                    if not Programs.Is_Member(
                                       Subprogram => Ret.PO_Entry.Sub,
                                       Of_Set     => Roots)
                                    then
                                       Programs.Add(To     => Roots,
                                                    Adding => Ret.PO_Entry.Sub);
                                                    
                                       Programs.Add_Root(
                                          Root => Ret.PO_Entry.Sub,
                                          To   => Program,
                                          Call => Ret.PO_Entry.Call);
                                    else
                                       Output_Error (
                                          Msg   => "Duplicate entry "
                                             & Ret.PO_Entry.Name.all,
                                          Locus => Output.Locus (
                                             Program_File => Program_Locus,
                                             Source_File  => TPOF_Name,
                                             Call_Path    => Name.all));
                                       raise TPOF_Error;
                                    end if;                                    
                                 else
                                    Both_Found := false;
                                 end if;

                                 Find(Name  => Ret.Barriered.Name.all,
                                      Msg   => "Barriered entry subprogram",
                                      Found => Found,
                                      Sub   => Ret.Barriered.Sub);

                                 if Found then
                                    if not Programs.Is_Member(
                                       Subprogram => Ret.Barriered.Sub,
                                       Of_Set     => Roots)
                                    then
                                       Programs.Add(To     => Roots,
                                                    Adding => Ret.Barriered.Sub);
                                                    
                                       Programs.Add_Root(
                                          Root => Ret.Barriered.Sub,
                                          To   => Program,
                                          Call => Ret.Barriered.Call);
                                    else
                                       Output_Error (
                                          Msg   => "Duplicate entry "
                                             & Ret.Barriered.Name.all,
                                          Locus => Output.Locus (
                                             Program_File => Program_Locus,
                                             Source_File  => TPOF_Name,
                                             Call_Path    => Name.all));
                                       raise TPOF_Error;
                                    end if;
                                 else
                                    Both_Found := false;
                                 end if;

                                 if Both_Found then

                                    -- Add PO_Entries to HRT object.
                                    Add(To     => HRT_Struct.PO_Entries,
                                        Adding => Ret.PO_Entry);
                                    Add(To     => HRT_Struct.PO_Entries,
                                        Adding => Ret.Barriered);

                                 else

                                    -- At least one entry was not found.
                                    Next_Token(Prog_Comments);
                                    return null;

                                 end if;

                                 if String_Vectors.Length(Comments) > 0 then
                                    -- Add comments, if any.
                                    Ret.Comments := new Name_List_T'
                                      (String_Vectors.To_Vector(Comments));

                                 end if;

                                 Next_Token(Prog_Comments);

                                 return Ret;
                              else
                                 Output_Error(
                                    Msg   => "End " & Name.all & " expected.",
                                    Locus => Output.Locus(
                                       Program_File => Program_Locus,
                                       Source_File  => TPOF_Name,
                                       Call_Path    => Name.all));
                              end if;
                           else
                              Output_Error(
                                 Msg   => "PO_Entry expected.",
                                 Locus => Output.Locus(
                                    Program_File => Program_Locus,
                                    Source_File  => TPOF_Name,
                                    Call_Path    => Name.all));
                           end if;
                        else
                           Output_Error(
                              Msg   => "Barriered_PO_Entry expected.",
                              Locus => Output.Locus(
                                 Program_File => Program_Locus,
                                 Source_File  => TPOF_Name,
                                 Call_Path    => Name.all));
                              
                        end if;
                     else
                        Output_Error(
                           Msg   => "PO_Entry expected.",
                           Locus => Output.Locus(
                              Program_File => Program_Locus,
                              Source_File  => TPOF_Name,
                              Call_Path    => Name.all));
                           
                     end if;

                     -- Skip this protected object
                     if Name.all /= Skip_Till_End then
                        Output_Warning(
                           Msg   => "PO_name at end does not match.",
                           Locus => Output.Locus (
                              Program_File => Program_Locus,
                              Source_File  => TPOF_Name,
                              Call_Path    => Name.all));
                     end if;

                     Next_Token(Prog_Comments);
                     return null;

                  when others =>

                     -- Skip this protected object
                     if Name.all /= Skip_Till_End then
                        Output_Warning(
                           Msg   => "PO_name at end does not match.",
                           Locus => Output.Locus (
                              Program_File => Program_Locus,
                              Source_File  => TPOF_Name,
                              Call_Path    => Name.all));
                     end if;

               end case;
            end if;

            Next_Token(Prog_Comments);

            return null;

         end;

      end Get_Resource_Or_Synchro;


      procedure Get_Definitions
      is
         Def : Def_T;
      begin
         loop
            case Tokenizer.ID(Analyzer) is
               when Thread_Tok =>
                  Def := Get_Thread;
               when Protected_Tok =>
                  Def := Get_Resource_Or_Synchro;
               when End_Tok =>
                  exit;
               when others =>
                  Output_Error(
                    Msg    => "Definition or end prog_name expected.",
                    Locus  => Output.Locus (
                       Program_File => Program_Locus,
                       Source_File  => TPOF_Name));
                  Next_Token(Prog_Comments);
            end case;
            if Def /= null then
               Def_Vectors.Append(To     => HRT_Struct.Definitions,
                                  Value  => Def);
            end if;

         end loop;

      end Get_Definitions;


      procedure Get_Program
      is
      begin
         Next_Token(Prog_Comments);

         if Tokenizer.ID(Analyzer) = Program_Tok then

            Next_Token(Prog_Comments);

            HRT_Struct.Prog_Name := new String'
              (Tokenizer.Lexeme(Analyzer));

            Next_Token(Prog_Comments);

            if Tokenizer.ID(Analyzer) = End_Tok then
               Output_Error(
                  Msg   => "No definitions found.",
                  Locus => Output.Locus (
                     Program_File => Program_Locus,
                     Source_File  => TPOF_Name));
            else
               Get_Definitions;
            end if;

            if Tokenizer.ID(Analyzer) = End_Tok then
               Next_Token(Prog_Comments);

               if HRT_Struct.Prog_Name.all /= Tokenizer.Lexeme(Analyzer) then
                  Output_Warning(
                     Msg   => "Program name at end does not match.",
                     Locus => Output.Locus(
                        Program_File => Program_Locus,
                        Source_File  => TPOF_Name));
               end if;

            else

               Output_Error(
                  Msg   => "End program expected.",
                  Locus => Output.Locus(
                     Program_File => Program_Locus,
                     Source_File  => TPOF_Name));
               raise TPOF_Syntax_Error;
            end If;

            if Errors > 0 then
               raise TPOF_Syntax_Error;
            end if;

         else
            Output_Error(
               Msg   => "Program expected.",
               Locus => Output.Locus(
                  Program_File => Program_Locus,
                  Source_File  => TPOF_Name));
            raise TPOF_Syntax_Error;
         end if;

         if String_Vectors.Length(Prog_Comments) > 0 then
            -- Add comments, if any.
            HRT_Struct.Comments := new Name_List_T'
              (String_Vectors.To_Vector(Prog_Comments));
         end if;

      exception
         when TPOF_Syntax_Error =>
            Output.Error(Text => "TPO file contained total of" &
                         Natural'Image(Errors) & " syntax errors.");
            raise;
      end Get_Program;


   begin

      -- Open the TPOF and read and parse it;

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => TPOF_Name);
      Ada.Text_IO.Set_Input (File);

      Tokenizer.Input_Feeder := OpenToken.Text_Feeder.Text_IO.Create;

      Get_Program;

      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Set_Input(Old_File.all);


      -- Check that the target program contains the entities
      -- named in the TPOF (error message and failure indication
      -- if some check fails):

      if not All_Found then
         Output.Error(
            Text => "TPO file contained subprograms"
               & " that where not found.");
         raise TPOF_Error;
      end if;

   exception
      when Ada.Text_IO.Name_Error =>
         Output.Error(Text => "TPO file " & TPOF_Name & " was not found.");
         raise TPOF_Error;

      when Ada.Text_IO.Use_Error | Ada.Text_IO.Status_Error =>
         Output.Error(Text => "TPO file " & TPOF_Name &
                      " could not be opened for reading.");
         raise TPOF_Error;

   end Get_TPOF;


end HRT.TPOF;
