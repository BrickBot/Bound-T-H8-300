-- Exec_Cmd (body) for Windows-NT
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
-- $Date: 2015/10/25 07:30:55 $
--
-- $Log: exec_cmd.adb,v $
-- Revision 1.7  2015/10/25 07:30:55  niklas
-- Moved to free licence.
--
-- Revision 1.6  2013/12/23 09:46:58  niklas
-- Corrected description of Read_Line: end-of-output does not
-- propagate an exception; instead, Is_EOF must be called after
-- Read_Line. Deleted the unused exception End_Of_Output and
-- normalized the name of IsEOF to Is_EOF.
--
-- Revision 1.5  2009-11-14 07:18:15  niklas
-- Corrected Close_Pipe to close the Read end, too, instead of
-- closing the Write end twice.
--
-- Revision 1.4  2005-07-25 20:18:29  Niklas
-- Added Exit_Code_T and related operations.
--
-- Revision 1.3  2005/04/14 09:35:47  Niklas
-- Changed Output.Errors to Output.Faults.
--
-- Revision 1.2  2003/01/30 16:08:08  holsti
-- Added 'use type' to satisfy Gnat 3.15.
--
-- Revision 1.1  2001/08/24 11:45:37  holsti
-- First version.
--


with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Output;

with System;
with Win32.Winnt;
with Win32.Winbase;
with Win32.Winerror;


package body Exec_Cmd is


   NUL : constant Character := Ada.Characters.Latin_1.NUL;
   CR  : constant Character := Ada.Characters.Latin_1.CR;
   LF  : constant Character := Ada.Characters.Latin_1.LF;

   CR_LF : constant String := (CR, LF);


   function Good (Result : Win32.BOOL) return Boolean
   --
   -- Whether the Win32 function result is good (no error).
   --
   is
      use type Win32.BOOL;
   begin
      return Result /= Win32.FALSE;
      -- Note that this is _not_ the same as "= Win32.TRUE".
   end Good;


   function Size_In_Bytes (Size_In_Bits : in Natural) return Win32.DWORD
   --
   -- Rounds a bit-size up to the nearest number of bytes (octets).
   --
   is
   begin
      return Win32.DWORD ((Size_In_Bits + 7) / 8);
   end Size_In_Bytes;


   function Normal_Exit (Status : Status_T) return Boolean
   is
   begin

      return Status = Success;

   end Normal_Exit;


   function Exit_Code (Status : Status_T) return Exit_Code_T
   is
   begin

      return Exit_Code_T (Status);

   end Exit_Code;


   procedure Create_Pipe (
      Pipe : out Pipe_T;
      Done : out Boolean)
   --
   -- Creates a pipe.
   --
   is

      Security : aliased Win32.Winbase.SECURITY_ATTRIBUTES;
      -- Security descriptor for the new pipe.
      -- Its main role is to ensure that the pipe's handles
      -- may be inherited by the child process.

   begin

      Security := (
         nLength              => Size_In_Bytes (Security'Size),
         lpSecurityDescriptor => System.Null_Address,
         bInheritHandle       => Win32.TRUE);
      -- The null value gives the default security descriptor of
      -- the parent process.

      Done := Good (
         Win32.Winbase.CreatePipe (
            hReadPipe        => Pipe.Read'Unchecked_Access,
            hWritePipe       => Pipe.Write'Unchecked_Access,
            lpPipeAttributes => Security'Unchecked_Access,
            nSize            => 0));

   end Create_Pipe;


   procedure Close_Pipe (
      Pipe : in out Pipe_T)
   --
   -- Closes a pipe (both handles).
   --
   is
   begin

      if not Good (Win32.Winbase.CloseHandle (Pipe.Write)) then

         Output.Fault (
            Location => "Exec_Cmd.Close_Pipe",
            Text     => "Could not close write-end of pipe");

      end if;

      if not Good (Win32.Winbase.CloseHandle (Pipe.Read)) then

         Output.Fault (
            Location => "Exec_Cmd.Close_Pipe",
            Text     => "Could not close read-end of pipe");

      end if;

   end Close_Pipe;


   procedure Disinherit (Handle : in out Win32.Winnt.HANDLE)
   --
   -- Ensure that this handle is not inherited by future child
   -- processes. Note that the implementation may have to change
   -- the handle itself (e.g. by duplicating it and closing the
   -- old handle).
   --
   is
      use type Win32.DWORD;

      Self : constant Win32.Winnt.HANDLE := Win32.Winbase.GetCurrentProcess;
      -- This process.

      Dup_Handle : aliased Win32.Winnt.HANDLE;
      -- A duplicate of the given handle.

   begin

      -- The important part of the following DuplicateHandle
      -- is the parameter bInheritHandle => Win32.FALSE:

      if Good (
         Win32.Winbase.DuplicateHandle (
            hSourceProcessHandle => Self,
            hSourceHandle        => Handle,
            hTargetProcessHandle => Self,
            lpTargetHandle       => Dup_Handle'Unchecked_Access,
            dwDesiredAccess      => 0,
            bInheritHandle       => Win32.FALSE,
            dwOptions            => Win32.Winnt.DUPLICATE_CLOSE_SOURCE
                                 or Win32.Winnt.DUPLICATE_SAME_ACCESS))
      then

         Handle := Dup_Handle;

      else

         Output.Fault (
            Location => "Exec_Cmd.Disinherit",
            Text     => "Could not duplicate handle");

      end if;

      -- In Windows NT, the function Win32.Winbase.SetHandleInformation
      -- would be a more direct way to make the handle non-inheritable.
      -- However, SetHandleInformation is not supported under Windows 95.

   end Disinherit;


   function Execute (
      Command : in String;
      Newline : in Newline_T)
   return Execution_T
   is

      Pipe_Created : Boolean;
      -- Whether a pipe could be created.

      Startup : aliased Win32.Winbase.STARTUPINFO;
      -- Startup-information for the child.
      -- Redirects the standard input and output of the
      -- child process to the pipes.

      Command_With_Nul : constant String := Command & NUL;
      -- The command with a NUL terminator.

      Exec : Execution_T;
      -- The child process in execution.


      procedure Fault (Text : in String)
      --
      -- Reports a fault.
      --
      is
      begin

         Output.Fault (
            Location => "Exec_Cmd.Execute",
            Text     => Text);

      end Fault;


   begin  -- Execute

      -- Make pipes to and from the child:

      Create_Pipe (
         Pipe => Exec.To_Child,
         Done => Pipe_Created);

      if not Pipe_Created then

         Fault ("Could not create pipe to " & Command);

         raise Execution_Error;

      end if;

      Create_Pipe (
         Pipe => Exec.From_Child,
         Done => Pipe_Created);

      if not Pipe_Created then

         Fault ("Could not create pipe from " & Command);

         Close_Pipe (Pipe => Exec.To_Child);

         raise Execution_Error;

      end if;

      -- Prevent the child from inheriting the pipe-ends it
      -- will not use:

      Disinherit (Handle => Exec.To_Child.Write);
      Disinherit (Handle => Exec.From_Child.Read);

      -- Define the pipes as the standard input and output
      -- for the child process, with the standard error channel
      -- inherited from the parent:

      Startup.dwFlags    := Win32.Winbase.STARTF_USESTDHANDLES;
      Startup.hStdInput  := Exec.To_Child.Read;
      Startup.hStdOutput := Exec.From_Child.Write;
      Startup.hStdError  :=
         Win32.Winbase.GetStdHandle (Win32.Winbase.STD_ERROR_HANDLE);

      -- Set other startup attributes:

      Startup.cb          := Size_In_Bytes (Startup'Size);
      Startup.lpReserved  := null;
      Startup.lpDesktop   := null;
      Startup.lpTitle     := null;
      Startup.cbReserved2 := 0;
      Startup.lpReserved2 := null;

      -- Spawn the child process:
      --
      -- The "null" parameters have the following meaning:
      --
      --   lpApplicationName
      --      The "module name" is the first word on the command line.
      --
      --   lpProcessAttributes, lpThreadAttributes
      --      The process and thread get default attributes and their
      --      handles are not inherited by (future) children.
      --
      --   lpEnvironment, lpCurrentDirectory
      --      Child has same environment and current dir as parent.
      --

      if not Good (
         Win32.Winbase.CreateProcess (
            lpApplicationName    => null,
            lpCommandLine        => Win32.Addr (Command_With_Nul),
            lpProcessAttributes  => null,
            lpThreadAttributes   => null,
            bInheritHandles      => Win32.TRUE,
            dwCreationFlags      => 0,
            lpEnvironment        => System.Null_Address,
            lpCurrentDirectory   => null,
            lpStartupInfo        => Startup'Unchecked_Access,
            lpProcessInformation => Exec.Child'Unchecked_Access))
      then

         Fault ("Execution of " & Command & " failed");

         Close_Pipe (Pipe => Exec.To_Child  );
         Close_Pipe (Pipe => Exec.From_Child);

         raise Execution_Error;

      end if;

      -- Close the pipe-ends that the parent will not use:

      if not Good (Win32.Winbase.CloseHandle (Exec.From_Child.Write)) then

         Fault ("Could not close unused write-end of pipe");

      end if;

      if not Good (Win32.Winbase.CloseHandle (Exec.To_Child.Read)) then

         Fault ("Could not close unused read-end of pipe");

      end if;

      -- Define the line separator:

      Exec.Newline := Newline;

      return Exec;

   end Execute;


   procedure End_Execution (Exec : in out Execution_T)
   is
      Unused : Status_T;
   begin
      End_Execution (Exec => Exec, Status => Unused);
   end End_Execution;


   procedure End_Execution (
      Exec   : in out Execution_T;
      Status :    out Status_T)
   is
      use type Win32.DWORD;
      use type Win32.Winnt.HANDLE;

      Event : Win32.DWORD;
      -- The even that terminates the "wait".

      Exit_Code : aliased Win32.DWORD;
      -- The exit code of the child process.


      procedure Fault (Text : in String)
      --
      -- Reports a fault.
      --
      is
      begin

         Output.Fault (
            Location => "Exec_Cmd.Execute",
            Text     => Text);

      end Fault;


   begin  -- End_Execution

      Exec.Valid := False;

      -- Close the pipes, if not already closed:

      if Exec.To_Child.Write /= Win32.Winbase.INVALID_HANDLE_VALUE then

         if not Good (Win32.Winbase.CloseHandle (Exec.To_Child.Write)) then

            Fault ("Could not close write-end of pipe");

         end if;

      end if;

      if not Good (Win32.Winbase.CloseHandle (Exec.From_Child.Read)) then

         Fault ("Could not close read-end of pipe");

      end if;

      -- Wait for the child process to terminate:

      Event := Win32.Winbase.WaitForSingleObject (
         hHandle        => Exec.Child.hProcess,
         dwMilliseconds => Win32.Winbase.INFINITE);

      if Event /= Win32.Winbase.WAIT_OBJECT_0 then

         Fault ("Child process terminated by unexpected event");

      end if;

      -- Get the exit-code:

      if not Good (
         Win32.Winbase.GetExitCodeProcess (
            hProcess   => Exec.Child.hProcess,
            lpExitCode => Exit_Code'Unchecked_Access))
      then

         Fault ("Could not get child process exit-code");

         Exit_Code := 0;

      end if;

      Status := Status_T (Exit_Code);

      -- Close the handles for thread and process:

      if not Good (
         Win32.Winbase.CloseHandle (
            hObject => Exec.Child.hThread))
      then

         Fault ("Could not close thread handle");

      end if;

      if not Good (
         Win32.Winbase.CloseHandle (
            hObject => Exec.Child.hProcess))
      then

         Fault ("Could not close process handle");

      end if;

   end End_Execution;


   procedure Read (
      Exec : in out Execution_T;
      Char :    out Character)
   is
      use type Win32.DWORD;

      Bytes : aliased Win32.DWORD;
      -- Number of bytes actually read.

      Error_Code : Win32.DWORD;
      -- Windows error-code, if ReadFile failed.

   begin

      Char := Ada.Characters.Latin_1.NUL;

      if Good (
         Win32.Winbase.ReadFile (
            hFile                => Exec.From_Child.Read,
            lpBuffer             => Char'Address,
            nNumberOfBytesToRead => 1,
            lpNumberOfBytesRead  => Bytes'Unchecked_Access,
            lpOverlapped         => null))
      then

         Exec.In_EOF := Bytes = 0;

      else

         Error_Code := Win32.Winbase.GetLastError;

         case Error_Code is

            when Win32.Winerror.ERROR_HANDLE_EOF =>

               Exec.In_EOF := True;

            when Win32.Winerror.ERROR_BROKEN_PIPE =>

               Output.Note (Text => "Broken pipe from child process");

               Exec.In_EOF := True;

            when others =>

               Output.Fault (
                  Location => "Exec_Cmd.Read",
                  Text     => "Read from pipe failed");

               raise Execution_Error;

         end case;

      end if;

   end Read;


   procedure Write (Exec : in out Execution_T; Str : in String)
   is

      Bytes : aliased Win32.DWORD;
      -- Number of bytes actually written (not used).

   begin

      if not Good (
         Win32.Winbase.WriteFile (
            hFile                  => Exec.To_Child.Write,
            lpBuffer               => Str(Str'First)'Address,
            nNumberOfBytesToWrite  => Str'Length,
            lpNumberOfBytesWritten => Bytes'Unchecked_Access,
            lpOverlapped           => null))
      then

         Output.Fault (
            Location => "Exec_Cmd.Write",
            Text     => "Write to pipe failed");

         raise Execution_Error;

      end if;

   end Write;


   procedure Read_Line (
      Exec : in out Execution_T;
      Buf  :    out Ada.Strings.Unbounded.Unbounded_String)
   is

      Char  : Character;
      -- One character read.

   begin

      Buf := Ada.Strings.Unbounded.Null_Unbounded_String;

      loop

         Read (Exec, Char);

         if      Exec.In_EOF
         or else Char = LF
         then

            return;

         elsif Char /= CR then

            Ada.Strings.Unbounded.Append (
               Source   => Buf,
               New_Item => Char);

         end if;

      end loop;

   end Read_Line;


   procedure Write_Line (Exec : in out Execution_T; Str : in String)
   is
   begin

      case Exec.Newline is
      when Unix_Style  => Write (Exec, Str & LF);
      when MSDOS_Style => Write (Exec, Str & CR_LF);
      end case;

   end Write_Line;


   procedure Write_End (Exec : in out Execution_T)
   is
   begin

      if not Good (Win32.Winbase.CloseHandle (Exec.To_Child.Write)) then

         Output.Fault (
            Location => "Exec_Cmd.Write_End",
            Text     => "Could not end output to child process");

      end if;

      Exec.To_Child.Write := Win32.Winbase.INVALID_HANDLE_VALUE;

   end Write_End;


   function Is_EOF (Exec : in Execution_T) return Boolean
   is
   begin

      return Exec.In_EOF;

   end Is_EOF;


   function Valid (Exec : in Execution_T) return Boolean
   is
   begin

      return Exec.Valid;

   end Valid;


end Exec_Cmd;
