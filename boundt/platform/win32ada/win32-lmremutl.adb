--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-lmremutl.adb,v $
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


with Stdarg.Impl;
use Stdarg.Impl;

package body Win32.Lmremutl is

   function "&" is new Stdarg.Concat (Win32.DWORD);
   function "&" is new Stdarg.Concat (Win32.Winnt.LPTSTR);
   function "&" is new Stdarg.Concat (LPDESC);

   function RxRemoteApi
     (
      ApiNumber : Win32.DWORD;
      UncServerName : Win32.Winnt.LPTSTR;
      ParmDescString : LPDESC;
      DataDesc16 : LPDESC;
      DataDesc32 : LPDESC;
      DataDescSmb : LPDESC;
      AuxDesc16 : LPDESC;
      AuxDesc32 : LPDESC;
      AuxDescSmb : LPDESC;
      Flags : Win32.DWORD;
      Args : Stdarg.ArgList := Stdarg.Empty)
     return Win32.DWORD is

      ArgList : Stdarg.ArgList := Stdarg.Empty & ApiNumber & UncServerName &
        ParmDescString & DataDesc16 & DataDesc32 & DataDescSmb &
        AuxDesc16 & AuxDesc32 & AuxDescSmb & Flags & Args;
      procedure C_RxRemoteApi;
      pragma Import (C, C_RxRemoteApi, "RxRemoteApi");       --  lmremutl.h:87

   begin
      return (Win32.DWORD (F_Varargs (C_RxRemoteApi'Address,
        ArgCount (ArgList),
        Address_of_First_Arg (ArgList))));
   end RxRemoteApi;

end Win32.Lmremutl;


