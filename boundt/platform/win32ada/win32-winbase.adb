--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-winbase.adb,v $
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
with Win32.Utils;

package body Win32.Winbase is

   function GlobalDiscard (hglbMem : Win32.Windef.HGLOBAL)
                          return Win32.Windef.HGLOBAL is
   begin
      return GlobalReAlloc (hglbMem, 0, GMEM_MOVEABLE);
   end GlobalDiscard;

   function LocalDiscard (hlocMem : Win32.Windef.HLOCAL)
                         return Win32.Windef.HLOCAL is
   begin
      return LocalReAlloc (hlocMem, 0, LMEM_MOVEABLE);
   end LocalDiscard;

   function MAKEINTATOM (wInteger : WORD) return Win32.Winnt.LPTSTR is
      function To_LPTSTR is new Ada.Unchecked_Conversion
        (DWORD, Win32.Winnt.LPTSTR);
   begin
      return To_LPTSTR (Win32.Utils.MAKELONG (Low => wInteger, High => 0));
   end MAKEINTATOM;

   function FormatMessageA (dwFlags : Win32.DWORD;
                            lpSource : Win32.LPCVOID;
                            dwMessageId : Win32.DWORD;
                            dwLanguageId : Win32.DWORD;
                            lpBuffer : Win32.LPSTR;
                            nSize : Win32.DWORD;
                            Arguments : Stdarg.ArgList := Stdarg.Empty)
                           return Win32.DWORD is

      function Doit (dwFlags : Win32.DWORD;
                     lpSource : Win32.LPCVOID;
                     dwMessageId : Win32.DWORD;
                     dwLanguageId : Win32.DWORD;
                     lpBuffer : Win32.LPSTR;
                     nSize : Win32.DWORD;
                     Arguments : access Stdarg.Impl.Param_Access)
                    return Win32.DWORD;
      pragma Import (Stdcall, Doit, "FormatMessageA");

      Param_Addr : aliased Stdarg.Impl.Param_Access :=
        Stdarg.Impl.Address_of_First_Arg (Arguments);
   begin
      return Doit (dwFlags, lpSource, dwMessageId, dwLanguageId,
                   lpBuffer, nSize, Param_Addr'Access);    --  strange
   end FormatMessageA;

   function FormatMessageW (dwFlags : Win32.DWORD;
                            lpSource : Win32.LPCVOID;
                            dwMessageId : Win32.DWORD;
                            dwLanguageId : Win32.DWORD;
                            lpBuffer : Win32.LPWSTR;
                            nSize : Win32.DWORD;
                            Arguments : Stdarg.ArgList := Stdarg.Empty)
                           return Win32.DWORD is

      function Doit (dwFlags : Win32.DWORD;
                     lpSource : Win32.LPCVOID;
                     dwMessageId : Win32.DWORD;
                     dwLanguageId : Win32.DWORD;
                     lpBuffer : Win32.LPWSTR;
                     nSize : Win32.DWORD;
                     Arguments : access Stdarg.Impl.Param_Access)
                    return Win32.DWORD;
      pragma Import (Stdcall, Doit, "FormatMessageW");

      Param_Addr : aliased Stdarg.Impl.Param_Access :=
        Stdarg.Impl.Address_of_First_Arg (Arguments);
   begin
      return Doit (dwFlags, lpSource, dwMessageId, dwLanguageId,
                   lpBuffer, nSize, Param_Addr'Access);    --  strange
   end FormatMessageW;

end Win32.Winbase;


