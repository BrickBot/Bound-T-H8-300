-- Exec_Cmd (body) for Unix
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
-- $Date: 2015/10/25 07:28:15 $
--
-- $Log: exec_cmd.adb,v $
-- Revision 1.8  2015/10/25 07:28:15  niklas
-- Moved to free licence.
--
-- Revision 1.7  2013/12/23 09:46:41  niklas
-- Corrected description of Read_Line: end-of-output does not
-- propagate an exception; instead, Is_EOF must be called after
-- Read_Line. Deleted the unused exception End_Of_Output and
-- normalized the name of IsEOF to Is_EOF.
--
-- Revision 1.6  2013-02-03 21:27:10  niklas
-- Added trace output of each execution argument.
--
-- Revision 1.5  2009-07-08 18:57:03  niklas
-- Corrected mode of parameter Read.Char.
--
-- Revision 1.4  2005-06-28 06:15:49  niklas
-- Corrected the handling of child process exit-status (Status_T).
-- The status is not identical to the exit code (Exit_Code_T), but
-- contains also the reason for the process termination and
-- perhaps other information.
--
-- Revision 1.3  2005/04/14 09:48:11  niklas
-- Changed Output.Errors to Output.Faults.
--
-- Revision 1.2  2004/04/25 07:04:42  niklas
-- First Tidorum version. To_String_List improved.
--
-- Revision 1.1  2001/09/28 09:09:37  holsti
-- First Unix version.
--


with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Exec_Cmd.Opt;
with Output;


package body Exec_Cmd is

   package U_Strings renames Ada.Strings.Unbounded;
   package F_Strings renames Ada.Strings.Fixed;


   Max_Params : constant Integer := 255;
   -- Maximum number of parameters that are delivered to executed program.


   type Return_T is new Interfaces.C.Int;
   --
   -- Return code from Unix system calls.


   subtype Pointer_T is System.Address;
   --
   -- C pointers.


   type Pipe_Ends_T is (Read_Pipe, Write_Pipe);
   type Pipe_T is array (Pipe_Ends_T) of File_Desc_T;
   --
   -- A UNIX pipe, a data channel between two processes.


   StdIn  : constant File_Desc_T := 0;
   StdOut : constant File_Desc_T := 1;
   StdErr : constant File_Desc_T := 2;
   --
   -- UNIX file handles for standard input, standard output
   -- and standard error.

   subtype Char_Array_T is Interfaces.C.Char_Array;
   --
   -- A string in C is a Char_Array. Ada Strings will have to
   -- converted to Char_Arrays if they are to be given to C functions
   -- as arguments.

   type Char_Array_Ref is access Char_Array_T;


   subtype String_Pointer_T is System.Address;
   type String_List_T is array (Positive range <>) of String_Pointer_T;
   --
   -- The system call EXEC will take a String_List_T as the arguments for
   -- the program to be EXEC'd.


   NullPointer_C : Pointer_T renames System.Null_Address;
   NullString_C  : String_Pointer_T renames System.Null_Address;
   --
   -- Constants needed in calling UNIX system calls.


   -------------------------------------------------------
   -- Import relevant system calls
   -------------------------------------------------------

   function Fork return ProcessID_T;
   --
   -- The UNIX system call "fork", which forks a new process
   -- almost identical to the current one.

   function Execvp (
      File : in String_Pointer_T;
      Argv : in String_List_T)
   return Return_T;
   --
   -- The UNIX system call "execvp", which overlays the calling
   -- process with a program file and executes it.


   procedure Process_Exit (Status : in Status_T);
   --
   -- The UNIX system call "exit", that exits the process and
   -- returns its exit value in Status.


   function Wait (Status : access Status_T) return ProcessID_T;
   --
   -- Blocks until the process ProcessID exits and then writes its exit
   -- status into Status.


   function Close (File : in File_Desc_T) return Return_T;
   --
   -- Close a file handle


   function Fsync (File : in File_Desc_T) return Return_T;
   --
   -- Syncronize (flush) file


   function Dup (File : in File_Desc_T) return File_Desc_T;
   --
   -- Duplicate an open file handle. The returned file handle points to
   -- the same file as the original.


   function Dup2 (
      File  : in File_Desc_T;
      File2 : in File_Desc_T)
   return File_Desc_T;
   --
   -- Duplicate file handle to defined handle file2


   function Write (
      File   : in File_Desc_T;
      Buffer : in Pointer_T;
      Bytes  : in Interfaces.C.Int)
   return Return_T;
   --
   -- Write Bytes bytes from Buffer into the file (or pipe) File.


   function Read (
      File   : in File_Desc_T;
      Buffer : in Pointer_T;
      Bytes  : in Interfaces.C.Int)
   return Return_T;
   --
   -- Read Bytes bytes from File to Buffer.


   function Make_Pipe (Pipe : access Pipe_T) return Return_T;
   --
   -- Construct a pipe.


   pragma Import (C, Wait,         "wait");
   pragma Import (C, Process_Exit, "exit");
   pragma Import (C, Execvp,       "execvp");
   pragma Import (C, Fork,         "fork");
   pragma Import (C, Close,        "close");
   pragma Import (C, Fsync,        "fsync");
   pragma Import (C, Dup,          "dup");
   pragma Import (C, Dup2,         "dup2");
   pragma Import (C, Make_Pipe,    "pipe");
   pragma Import (C, Read,         "read");
   pragma Import (C, Write,        "write");


   --
   --    Ada interfaces routines
   --


   -- Assumptions about Status_T:
   -- - Bits 0 .. 6 are:
   --      zero for normal exit
   --      7F for "stopped"
   --      signal number for signalled process.
   -- - Bits 8 .. 15 are the exit code, for normal exit.


   function Normal_Exit (Status : Status_T) return Boolean
   is
      use Interfaces.C;
   begin

      return (unsigned (Status) and 127) = 0;

   end Normal_Exit;


   function Exit_Code (Status : Status_T) return Exit_Code_T
   is
      use Interfaces.C;
   begin

      return Exit_Code_T ((unsigned (Status) / 256) and 255);

   end Exit_Code;


   function To_String_List (S : String) return String_List_T
   --
   -- The list of blank-separated arguments from S, with one null
   -- string to terminate the list.
   --
   is

      Count : Natural := 0;
      -- The number of arguments scanned from S.

      List : String_List_T(1 .. Max_Params + 1);
      -- Collects the result, as List(1 .. Count).
      -- The +1 is for the Null_String.

      First : Positive;
      -- The first character (left) to be scanned in S.

      Last : Natural := S'First - 1;
      -- The last character scanned in S.

      New_S : Char_Array_Ref;
      -- A new argument.

   begin

      Argument_Loop: loop

         -- Skip leading blanks:

         Blank_Loop: loop

            First := Last + 1;

            exit Argument_Loop when First > S'Last;

            exit Blank_Loop when S(First) /= ' ';

            Last := First;

         end loop Blank_Loop;

         -- S(First) is the start of an argument.

         Last := F_Strings.Index(S(First .. S'Last), " ");

         if Last = 0 then
            -- No more blanks in S.

            Last := S'Last;

         else
            -- S(Last) is the first blank, not to be included
            -- in the argument.

            Last := Last - 1;

         end if;

         -- S(First .. Last) is an argument.

         if Opt.Trace then

            Output.Trace (
                 "Execution argument"
               & Natural'Image (Count)
               & Output.Field_Separator
               & S(First .. Last));

         end if;

         if Count >= Max_Params then

            Output.Fault (
               Location => "Exec_Cmd.To_String_List",
               Text =>
                   "Command has more than"
                  & Natural'Image (Max_Params)
                  & " arguments; rest ignnored");

            exit Argument_Loop;

         end if;

         -- Add the argument to the list:

         New_S := new Char_Array_T'(Interfaces.C.To_C(S(First .. Last)));

         Count := Count + 1;

         List(Count) := New_S.all'Address;

      end loop Argument_Loop;

      -- Add the terminating null argument:

      Count := Count +1;
      List(Count) := NullString_C;

      return List(1..Count);

   end To_String_List;


   function Execute (Command : in String)
   return Execution_T
   is
      RetValue : Return_T;
      Pin      : aliased Pipe_T;
      Pout     : aliased Pipe_T;

      Exec     : Execution_T;

      Arg_List : String_List_T := To_String_List(Command);

      procedure Fault (Text : in String)
      --
      -- Reports a fault and raises Execution_Error.
      --
      is
      begin

         Output.Fault (
            Location => "Exec_Cmd.Execute",
            Text     => Text);

         raise Execution_Error;

      end Fault;

   begin  -- Execute

      Exec.In_EOF := FALSE;

      if Make_Pipe(Pin'Access) < 0 then

         Fault ("Could not create input pipe");

      end if;

      if Make_Pipe(Pout'Access) < 0 then

         Fault ("Could not create output pipe");

      end if;

      Exec.P_ID := FORK;

      if Exec.P_ID < 0 then

         Fault ("Could not fork");

      elsif Exec.P_ID = 0 then
         -- This is the child process

         -- Duplicate pipe ends to stdin and stdout:

         if Dup2(Pin(Read_Pipe), StdIn) < 0 then

            Fault ("Could not duplicate input file descriptor");

         end if;

         if Dup2(Pout(Write_Pipe), StdOut) < 0 then

            Fault ("Could not duplicate output file descriptor");

         end if;

         RetValue := Close(Pin(Read_Pipe));
         RetValue := Close(Pin(Write_Pipe));
         RetValue := Close(Pout(Read_Pipe));
         RetValue := Close(Pout(Write_Pipe));

         if Execvp (
            File => Arg_List(1),
            Argv => Arg_List) < 0
         then

            Fault ("Execution of " & Command & " failed");

         end if;

         -- This point should never be reached.

      else
         -- This is the parent process.

         RetValue := Close(Pin(Read_Pipe));
         RetValue := Close(Pout(Write_Pipe));

         Exec.FD_Out := Pin(Write_Pipe);
         Exec.FD_In  := Pout(Read_Pipe);
         -- Pipe in and pipe out considered from child process' point of
         -- view, therefore they appear here the other way around.

      end if;

      return Exec;

   end Execute;


   procedure End_Execution (Exec : in out Execution_T)
   is

      Status  : aliased Status_T;
      -- Exit status from the child.

      Ret_Val : Return_T;
      -- Unused return values from pipe closing.

   begin

      Ret_Val := Close(Exec.FD_Out);
      Ret_Val := Close(Exec.FD_In);

      Exec.P_ID := Wait (Status => Status'Access);
      -- Waits for process P_ID to end

      if not Normal_Exit (Status) then

         Output.Fault (
            Location => "Exec_Cmd.End_Execution",
            Text     =>
                 "Child process exit-status"
               & Status_T'Image (Status));

         raise Execution_Error;

      end if;

      Exec.Valid := False;

   end End_Execution;


   procedure End_Execution (
      Exec   : in out Execution_T;
      Status :    out Status_T)
   is

      Ret_Val : Return_T;
      -- Unused return values from pipe closing.

      Stat : aliased Status_T;
      -- Local status, for accessibility.

   begin

      Ret_Val := Close(Exec.FD_Out);
      Ret_Val := Close(Exec.FD_In);

      Exec.P_ID := Wait (Status => Stat'Access);

      Exec.Valid := False;

      Status := Stat;

   end End_Execution;


   procedure Read (
      Exec : in out Execution_T;
      Char :    out Character)
   is
      Ret_Value : Return_T;
   begin

      Ret_Value := Read (Exec.FD_In, Char'Address, 1);

      if Ret_Value < 0 then

         Output.Fault (
            Location => "Exec_Cmd.Read",
            Text     => "Read from pipe unsuccessful");

         raise Execution_Error;

      end if;

      Exec.In_EOF := Ret_Value = 0;

   end Read;


   procedure Write (Exec : in out Execution_T; Str : in String)
   is
      Ret   : Return_T;

   begin

      Ret := Write(Exec.FD_Out, Str'Address, Str'Length);

      if Ret < 0 then

         Output.Fault (
            Location => "Exec_Cmd.Write",
            Text     => "Write to pipe unsuccessful");

         raise Execution_Error;

      end if;

   end Write;


   procedure Read_Line (
      Exec : in out Execution_T;
      Buf  :    out U_Strings.Unbounded_String)
   is
      Char  : Character;
   begin

      Buf := U_Strings.Null_Unbounded_String;

      loop

         Read(Exec, Char);

         if Exec.In_EOF or Char = Ascii.LF then
            return;
         end if;

         U_Strings.Append (Source => Buf, New_Item => Char);

      end loop;

   end Read_Line;


   procedure Write_Line (Exec : in out Execution_T; Str : in String)
   is
   begin

      Write(Exec, Str & Ascii.Lf);

   end Write_Line;


   procedure Write_End (Exec : in out Execution_T)
   is
      Ret : Return_T;
      -- Return code from close.
   begin

      Ret := Close (Exec.FD_Out);

      if Ret /= 0 then

         Output.Fault (
            Location => "Exec_Cmd.Write_End",
            Text     =>
                 "Error "
               & Return_T'Image (Ret)
               & " when closing output to child");

      end if;

   end Write_End;


   function Is_EOF (Exec : in Execution_T) return Boolean
   is
   begin

      return Exec.In_EOF;

   end Is_EOF;


   function Valid(Exec : in Execution_T) return Boolean
   is
   begin

      return Exec.Valid;

   end Valid;


end Exec_Cmd;
