--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-winuser.adb,v $
--  $Revision: 1.1 $ $Date: 2009-08-31 07:20:39 $ $Author: niklas $
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

package body Win32.Winuser is

   function MAKEINTRESOURCEA (wInteger : WORD) return LPSTR is
      --  winuser.h:102
      function To_LPSTR is new Ada.Unchecked_Conversion (DWORD, LPSTR);
   begin
      return To_LPSTR (Win32.Utils.MAKELONG (wInteger, 0));
   end MAKEINTRESOURCEA;

   function MAKEINTRESOURCEW (wInteger : WORD) return LPWSTR is
      --  winuser.h:103
      function To_LPWSTR is new Ada.Unchecked_Conversion (DWORD, LPWSTR);
   begin
      return To_LPWSTR (Win32.Utils.MAKELONG (wInteger, 0));
   end MAKEINTRESOURCEW;

   procedure POINTSTOPOINT (PT : out Win32.Windef.POINT;
                            PTS : in  Win32.Windef.POINTS) is
   begin
      --  This looks real different from the C, but I think it does
      --  the same thing!
      PT.x := LONG (PTS.x);
      PT.y := LONG (PTS.y);
   end POINTSTOPOINT;

   function POINTTOPOINTS (PT : Win32.Windef.POINT) return Win32.Windef.POINTS
   is
      PTS : Win32.Windef.POINTS;
   begin
      PTS.x := SHORT (PT.x);
      PTS.y := SHORT (PT.y);
      return PTS;
   end POINTTOPOINTS;

   function MAKEWPARAM (L, H : WORD) return WPARAM is
   begin
      return WPARAM (Win32.Windef.MAKELONG (Low => L, High => H));
   end MAKEWPARAM;

   function MAKELPARAM (L, H : WORD) return LPARAM is
   begin
      return LPARAM (Win32.Windef.MAKELONG (Low => L, High => H));
   end MAKELPARAM;

   function MAKELRESULT (L, H : WORD) return LRESULT is
   begin
      return LRESULT (Win32.Windef.MAKELONG (Low => L, High => H));
   end MAKELRESULT;

   function ExitWindows (dwReserved : DWORD;
                         uReturnCode : UINT) return Win32.BOOL is
      use type Win32.DWORD;
   begin
      if ExitWindowsEx (EWX_LOGOFF, -1) = 0 then
         return 0;
      else
         return 1;
      end if;
   end ExitWindows;

   function CreateWindowA (lpClassName : Win32.LPCSTR;
                           lpWindowName : Win32.LPCSTR;
                           dwStyle : Win32.DWORD;
                           X : Win32.INT;
                           Y : Win32.INT;
                           nWidth : Win32.INT;
                           nHeight : Win32.INT;
                           hWndParent : Win32.Windef.HWND;
                           hMenu : Win32.Windef.HMENU;
                           hInstance : Win32.Windef.HINSTANCE;
                           lpParam : Win32.LPVOID)
                          return Win32.Windef.HWND is

   begin
      return CreateWindowExA (0, lpClassName, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, hMenu,
        hInstance, lpParam);
   end CreateWindowA;

   function CreateWindowW (lpClassName : Win32.LPCWSTR;
                           lpWindowName : Win32.LPCWSTR;
                           dwStyle : Win32.DWORD;
                           X : Win32.INT;
                           Y : Win32.INT;
                           nWidth : Win32.INT;
                           nHeight : Win32.INT;
                           hWndParent : Win32.Windef.HWND;
                           hMenu : Win32.Windef.HMENU;
                           hInstance : Win32.Windef.HINSTANCE;
                           lpParam : Win32.LPVOID)
                          return Win32.Windef.HWND is
   begin
      return CreateWindowExW (0, lpClassName, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, hMenu,
        hInstance, lpParam);
   end CreateWindowW;

   function CreateDialogA (hInstance : Win32.Windef.HINSTANCE;
                           lpTemplateName : Win32.LPCSTR;
                           hWndParent : Win32.Windef.HWND;
                           lpDialogFunc : DLGPROC)
                          return Win32.Windef.HWND is
   begin
      return CreateDialogParamA (hInstance, lpTemplateName,
        hWndParent, lpDialogFunc, 0);
   end CreateDialogA;

   function CreateDialogW (hInstance : Win32.Windef.HINSTANCE;
                           lpTemplateName : Win32.LPCWSTR;
                           hWndParent : Win32.Windef.HWND;
                           lpDialogFunc : DLGPROC)
                          return Win32.Windef.HWND is
   begin
      return CreateDialogParamW (hInstance, lpTemplateName,
        hWndParent, lpDialogFunc, 0);
   end CreateDialogW;

   function CreateDialogIndirectA (hInstance : Win32.Windef.HINSTANCE;
                                   lpTemplate : LPCDLGTEMPLATEA;
                                   hWndParent : Win32.Windef.HWND;
                                   lpDialogFunc : DLGPROC)
                                  return Win32.Windef.HWND is
   begin
      return CreateDialogIndirectParamA
        (hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
   end CreateDialogIndirectA;

   function CreateDialogIndirectW (hInstance : Win32.Windef.HINSTANCE;
                                   lpTemplate : LPCDLGTEMPLATEW;
                                   hWndParent : Win32.Windef.HWND;
                                   lpDialogFunc : DLGPROC)
                                  return Win32.Windef.HWND is
   begin
      return CreateDialogIndirectParamW
        (hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
   end CreateDialogIndirectW;

   function DialogBoxA (hInstance : Win32.Windef.HINSTANCE;
                        lpTemplateName : Win32.LPCSTR;
                        hWndParent : Win32.Windef.HWND;
                        lpDialogFunc : DLGPROC)
                       return Win32.INT is
   begin
      return DialogBoxParamA (hInstance, lpTemplateName,
        hWndParent, lpDialogFunc, 0);
   end DialogBoxA;

   function DialogBoxW (hInstance : Win32.Windef.HINSTANCE;
                        lpTemplateName : Win32.LPCWSTR;
                        hWndParent : Win32.Windef.HWND;
                        lpDialogFunc : DLGPROC)
                       return Win32.INT is
   begin
      return DialogBoxParamW (hInstance, lpTemplateName,
        hWndParent, lpDialogFunc, 0);
   end DialogBoxW;

   function DialogBoxIndirectA (hInstance : Win32.Windef.HINSTANCE;
                                hDialogTemplate : LPCDLGTEMPLATEA;
                                hWndParent : Win32.Windef.HWND;
                                lpDialogFunc : DLGPROC)
                               return Win32.INT is
   begin
      return DialogBoxIndirectParamA (hInstance, hDialogTemplate,
        hWndParent, lpDialogFunc, 0);
   end DialogBoxIndirectA;

   function DialogBoxIndirectW (hInstance : Win32.Windef.HINSTANCE;
                                hDialogTemplate : LPCDLGTEMPLATEW;
                                hWndParent : Win32.Windef.HWND;
                                lpDialogFunc : DLGPROC)
                               return Win32.INT is
   begin
      return DialogBoxIndirectParamW (hInstance, hDialogTemplate,
        hWndParent, lpDialogFunc, 0);
   end DialogBoxIndirectW;

   function GetWindowTask (H : Win32.Windef.HWND) return Win32.Winnt.HANDLE is
      Res : DWORD;
      function To_Handle is new Ada.Unchecked_Conversion
        (DWORD, Win32.Winnt.HANDLE);
   begin
      Res := GetWindowThreadProcessId (H, null);
      return To_Handle (Res);
   end GetWindowTask;

   function DefHookProc (nCode : Win32.INT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM;
                         phhk : access Win32.Windef.HHOOK)
                        return Win32.LRESULT is
   begin
      return CallNextHookEx (phhk.all, nCode, wParam, lParam);
   end DefHookProc;

   function CopyCursor (hcur : Win32.Windef.HCURSOR)
                       return Win32.Windef.HCURSOR is
   begin
      return Win32.Windef.HCURSOR (CopyIcon (Win32.Windef.HICON (hcur)));
   end CopyCursor;

   function wsprintfA
     (lpOut : LPSTR;
      lpFmt : LPCSTR;
      arglist : Stdarg.ArgList := Stdarg.Empty) return Win32.INT is
      --  winuser.h:155

      use Stdarg, Stdarg.Impl;

      function "&" is new Stdarg.Concat (LPSTR);
      function "&" is new Stdarg.Concat (LPCSTR);

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & lpOut & lpFmt & arglist;

      function C_wsprintfA return Win32.INT;
      pragma Import (C, C_wsprintfA, "wsprintfA");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_wsprintfA'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));

   end wsprintfA;

   function wsprintfW
     (lpOut : LPWSTR;
      lpFmt : LPCWSTR;
      arglist : Stdarg.ArgList := Stdarg.Empty) return Win32.INT is
      --  winuser.h:155

      use Stdarg, Stdarg.Impl;

      function "&" is new Stdarg.Concat (LPWSTR);
      function "&" is new Stdarg.Concat (LPCWSTR);

      Complete_Args : Stdarg.ArgList :=
        Stdarg.Empty & lpOut & lpFmt & arglist;

      function C_wsprintfW return Win32.INT;
      pragma Import (C, C_wsprintfW, "wsprintfW");

      function To_INT is new Ada.Unchecked_Conversion
        (Stdarg.C_Param, Win32.INT);
   begin
      return To_INT (F_Varargs
        (C_wsprintfW'Address,
        ArgCount (Complete_Args),
        Address_of_First_Arg (Complete_Args)));

   end wsprintfW;


   function wvsprintfA (lpOut : Win32.LPSTR;
                        lpFmt : Win32.LPCSTR;
                        arglist : Stdarg.ArgList := Stdarg.Empty)
                       return Win32.INT is

      function doit (lpOut : Win32.LPSTR;
                     lpFmt : Win32.LPCSTR;
                     arglist : Stdarg.Impl.Param_Access)
                    return Win32.INT;

      pragma Import (Stdcall, doit, "wvsprintfA");
   begin
      return doit (lpOut, lpFmt, Stdarg.Impl.Address_of_First_Arg (arglist));
   end wvsprintfA;

   function wvsprintfW (lpOut : Win32.LPWSTR;
                        lpFmt : Win32.LPCWSTR;
                        arglist : Stdarg.ArgList := Stdarg.Empty)
                       return Win32.INT is

      function doit (lpOut : Win32.LPWSTR;
                     lpFmt : Win32.LPCWSTR;
                     arglist : Stdarg.Impl.Param_Access)
                    return Win32.INT;

      pragma Import (Stdcall, doit, "wvsprintfW");
   begin
      return doit (lpOut, lpFmt, Stdarg.Impl.Address_of_First_Arg (arglist));
   end wvsprintfW;

end Win32.Winuser;


