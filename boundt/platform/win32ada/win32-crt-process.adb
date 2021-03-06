--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-crt-process.adb,v $
--  $Revision: 1.1 $ $Date: 2009-08-31 07:20:38 $ $Author: niklas $
-------------------------------------------------------------------------------
--
--  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS"
--  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
--  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY
--  AND/OR FITNESS FOR A PARTICULAR PURPOSE.  The user assumes the
--  entire risk as to the accuracy and the use of this file.
--
--  Copyright (c) Intermetrics, Inc. 1995
--  Royalty-free, unlimited, worldwide, non-exclusive use, modification,
--  reproduction and further distribution of this file is permitted.
--
-------------------------------------------------------------------------------


with Ada.Unchecked_Conversion;
with Stdarg.Impl;
with Stdarg.Inst;

package body Win32.crt.Process is

   use Stdarg, Stdarg.Impl, Stdarg.Inst;

   function "&" is new Stdarg.Concat (Win32.PCSTR);

   Null_Ptr : constant Win32.PCSTR := null;

   function execl
     (cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty)
     return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & cmdname & args & Null_Ptr;

      procedure C_Execl;
      pragma Import (C, C_Execl, "_execl");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_Execl'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));
   end execl;

   function execle
     (cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty;
      env : in Stdarg.ArgList := Stdarg.Empty)
     return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & cmdname &
        args & Null_Ptr &
        env  & Null_Ptr;

      procedure C_execle;
      pragma Import (C, C_execle, "_execle");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_execle'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));
   end execle;

   function execlp
     (cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty)
     return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & cmdname & args & Null_Ptr;

      procedure C_execlp;
      pragma Import (C, C_execlp, "_execlp");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_execlp'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));
   end execlp;

   function execlpe
     (cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty;
      env : in Stdarg.ArgList := Stdarg.Empty)
     return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & cmdname &
        args & Null_Ptr &
        env  & Null_Ptr;

      procedure C_execlpe;
      pragma Import (C, C_execlpe, "_execlpe");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_execlpe'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));
   end execlpe;

   function spawnl
     (mode : in Win32.INT;
      cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty) return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & mode & cmdname &
        args & Null_Ptr;

      procedure C_spawnl;
      pragma Import (C, C_spawnl, "_spawnl");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_spawnl'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));

   end spawnl;

   function spawnle
     (mode : in Win32.INT;
      cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty;
      env : in Stdarg.ArgList := Stdarg.Empty) return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & mode & cmdname &
        args & Null_Ptr &
        env  & Null_Ptr;

      procedure C_spawnle;
      pragma Import (C, C_spawnle, "_spawnle");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_spawnle'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));

   end spawnle;

   function spawnlp
     (mode : in Win32.INT;
      cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty) return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & mode & cmdname &
        args & Null_Ptr;

      procedure C_spawnlp;
      pragma Import (C, C_spawnlp, "_spawnlp");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_spawnlp'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));

   end spawnlp;

   function spawnlpe
     (mode : in Win32.INT;
      cmdname : in Win32.PCSTR;
      args : in Stdarg.ArgList := Stdarg.Empty;
      env : in Stdarg.ArgList := Stdarg.Empty) return Win32.INT is

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & mode & cmdname &
        args & Null_Ptr &
        env  & Null_Ptr;

      procedure C_spawnlpe;
      pragma Import (C, C_spawnlpe, "_spawnlpe");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_spawnlpe'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));
   end spawnlpe;

end Win32.crt.Process;


