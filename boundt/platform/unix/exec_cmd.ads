-- Exec_Cmd (decl) for Unix
--
-- This package defines procedures for executing an external program
-- as a child process in a UNIX environment, with the ability to
-- write input to the program and read output from the program.
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
-- $Date: 2015/10/25 07:28:15 $
--
-- $Log: exec_cmd.ads,v $
-- Revision 1.4  2015/10/25 07:28:15  niklas
-- Moved to free licence.
--
-- Revision 1.3  2013/12/23 09:46:41  niklas
-- Corrected description of Read_Line: end-of-output does not
-- propagate an exception; instead, Is_EOF must be called after
-- Read_Line. Deleted the unused exception End_Of_Output and
-- normalized the name of IsEOF to Is_EOF.
--
-- Revision 1.2  2005-06-28 06:15:49  niklas
-- Corrected the handling of child process exit-status (Status_T).
-- The status is not identical to the exit code (Exit_Code_T), but
-- contains also the reason for the process termination and
-- perhaps other information.
--
-- Revision 1.1  2001/09/28 09:09:37  holsti
-- First Unix version.
--


with Interfaces.C;
with Ada.Strings.Unbounded;


package Exec_Cmd is


   type Execution_T is private;
   --
   -- Represents a child process to which input can be sent (written)
   -- and from which output can be received (read).
   --
   -- Initialized by the procedure Execute, finalized by the
   -- procedure End_Execution (either variant).


   type Status_T is new Interfaces.C.Int;
   --
   -- The termination status of a child process.
   -- This contains the reason for termination (normal exit, signal,
   -- etc.) and the exit code (in case of normal exit). Note that a
   -- "normal exit" here includes cases where the exit code is nonzero,
   -- which the process may consider failure exits.


   type Exit_Code_T is range 0 .. 255;
   --
   -- The exit code of a process that ended by normal exit.


   function Normal_Exit (Status : Status_T) return Boolean;
   --
   -- Whether the Status shows a normal exit.
   -- Corresponds to the C macro WIFEXITED.


   function Exit_Code (Status : Status_T) return Exit_Code_T;
   --
   -- The exit code in the Status. This function should be used
   -- only if Normal_Exit (Status) is True.
   -- Corresponds to the C macro WEXITSTATUS.


   Execution_Error : exception;
   --
   -- Signals an unrecoverable error during child process creation.


   function Execute (Command : in String)
   return Execution_T;
   --
   -- Starts a child process defined by the given shell command.
   -- Returns an execution-object for communication with the child.


   procedure End_Execution (Exec : in out Execution_T);
   --
   -- Terminates the child process.
   -- Raises Execution_Error if there is a problem.


   procedure End_Execution (
      Exec   : in out Execution_T;
      Status :    out Status_T);
   --
   -- Terminates the child processor and returns its exit status.


   procedure Read_Line (
      Exec : in out Execution_T;
      Buf  :    out Ada.Strings.Unbounded.Unbounded_String);
   --
   -- Reads a line of output from the execution.
   -- Afterwards, the function Is_EOF (see below) shows if
   -- something could be read, of if the channel indicates
   -- end-of-file.


   procedure Write     (Exec : in out Execution_T; Str : in String);
   procedure Write_Line(Exec : in out Execution_T; Str : in String);
   --
   -- Writes input text, the latter with a newline at the end, to
   -- the execution.


   procedure Write_End (Exec : in out Execution_T);
   --
   -- Terminates the input to the execution. In effect, "closes"
   -- the stream of input data to the execution.


   function Is_EOF(Exec : in Execution_T) return Boolean;
   --
   -- Whether the output from the execution has ended.
   -- To be called after Read_Line to check if some output
   -- was available or not.


   function Valid(Exec : in Execution_T) return Boolean;
   --
   -- Whether the execution is running.


private


   type ProcessID_T is new Interfaces.C.Int;
   --
   -- A process identifier.


   type File_Desc_T is new Interfaces.C.Int;
   --
   -- A file (channel) descriptor.


   type Execution_T is record
      P_ID    : ProcessID_T;
      FD_In   : File_Desc_T;
      FD_Out  : File_Desc_T;
      In_EOF  : Boolean := False;
      Valid   : Boolean := True;
   end record;


end Exec_Cmd;
