--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-wincon.adb,v $
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

package body Win32.Wincon is

   --  Interface(C) function returning struct
   function GetLargestConsoleWindowSize
     (hConsoleOutput : Win32.Winnt.HANDLE) return COORD is

      function GetLargestConsoleWindowSize
        (hConsoleOutput : Win32.Winnt.HANDLE) return Win32.DWORD;
      pragma Import (Stdcall, GetLargestConsoleWindowSize,
                       "GetLargestConsoleWindowSize");
      function ToCoord is new Ada.Unchecked_Conversion (Win32.DWORD, COORD);
   begin
      return ToCoord (GetLargestConsoleWindowSize (hConsoleOutput));
   end GetLargestConsoleWindowSize;

end Win32.Wincon;


