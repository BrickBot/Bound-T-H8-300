--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-glu.adb,v $
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

package body Win32.Glu is

   function gluErrorStringWIN (errCode : Win32.Gl.GLenum)
                              return Win32.LPCWSTR is       --  glu.h:56

      function To_LPCWSTR is new Ada.Unchecked_Conversion (Win32.PCBYTE,
        Win32.LPCWSTR);
   begin
      return To_LPCWSTR (gluErrorString (errCode));
   end gluErrorStringWIN;

end Win32.Glu;




