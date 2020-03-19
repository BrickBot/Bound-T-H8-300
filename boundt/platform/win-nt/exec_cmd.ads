-- Exec_Cmd (decl) for Windows-NT
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
-- $Revision: 1.4 $
-- $Date: 2015/10/25 07:30:55 $
--
-- $Log: exec_cmd.ads,v $
-- Revision 1.4  2015/10/25 07:30:55  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013/12/23 09:46:58  niklas
-- Corrected description of Read_Line: end-of-output does not
-- propagate an exception; instead, Is_EOF must be called after
-- Read_Line. Deleted the unused exception End_Of_Output and
-- normalized the name of IsEOF to Is_EOF.
--
-- Revision 1.2  2005-07-25 20:18:29  Niklas
-- Added Exit_Code_T and related operations.
--
-- Revision 1.1  2001/08/24 11:45:37  holsti
-- First version.
--


with Ada.Strings.Unbounded;
with Win32.Winnt;
with Win32.Winbase;


package Exec_Cmd is


   type Execution_T is private;
   --
   -- Represents a child process to which input can be sent (written)
   -- and from which output can be received (read).
   --
   -- Initialized by the procedure Execute, finalized by the
   -- procedure End_Execution (either variant).


   type Status_T is new Win32.DWORD;
   --
   -- The termination status of a child process, as defined by
   -- the operating system (MS Windows).


   type Exit_Code_T is new Status_T;
   --
   -- The exit code of a child process, showing success or failure
   -- as defined by the application.


   Success : constant Status_T := Status_T (Win32.Winnt.STATUS_WAIT_0);
   --
   -- The status value that represents successful termination of
   -- the child process (TBC).


   function Normal_Exit (Status : Status_T) return Boolean;
   --
   -- Whether the Status code shows a normal exit.


   function Exit_Code (Status : Status_T) return Exit_Code_T;
   --
   -- The exit code in the Status. This function should be used
   -- only if Normal_Exit (Status) is True.


   type Newline_T is (Unix_Style, MSDOS_Style);
   --
   -- Defines the way that lines are separated (terminated)
   -- in textual communication with (to) the child process.


   function Execute (
      Command : in String;
      Newline : in Newline_T)
   return Execution_T;
   --
   -- Starts a child process defined by the given shell command.
   -- Returns an execution-object for communication with the child.
   -- A line-separator style is specified for textual communication.


   procedure End_Execution (Exec : in out Execution_T);
   --
   -- Terminates the child process.
   -- Raises Execution_Error if there is a problem.


   procedure End_Execution (
      Exec   : in out Execution_T;
      Status :    out Status_T);
   --
   -- Terminates the child process and returns its exit status.


   procedure Read_Line (
      Exec : in out Execution_T;
      Buf  :    out Ada.Strings.Unbounded.Unbounded_String);
   --
   -- Reads a line of output from the execution.
   -- Afterwards, the function Is_EOF (see below) shows if
   -- something could be read, of if the channel indicates
   -- end-of-file.


   procedure Write      (Exec : in out Execution_T; Str : in String);
   procedure Write_Line (Exec : in out Execution_T; Str : in String);
   --
   -- Writes input text, the latter with a newline at the end, to
   -- the execution.


   procedure Write_End (Exec : in out Execution_T);
   --
   -- Terminates the input to the execution. In effect, "closes"
   -- the stream of input data to the execution.


   function Is_EOF (Exec : in Execution_T) return Boolean;
   --
   -- Whether the output from the execution has ended.
   -- To be called after Read_Line to check if some output
   -- was available or not.


   function Valid (Exec : in Execution_T) return Boolean;
   --
   -- Whether the execution is running.


   Execution_Error : exception;
   --
   -- Signals an unrecoverable error during child process creation.


private

   type Pipe_T is record
      Write : aliased Win32.Winnt.HANDLE;
      Read  : aliased Win32.Winnt.HANDLE;
   end record;
   --
   -- A Win32 (anonymous) pipe, with a handle for writing and
   -- a handle for reading.


   type Execution_T is record
      Valid      : Boolean := True;
      Child      : aliased Win32.Winbase.PROCESS_INFORMATION;
      To_Child   : Pipe_T;
      From_Child : Pipe_T;
      In_EOF     : Boolean := False;
      Newline    : Newline_T;
   end record;


end Exec_Cmd;

