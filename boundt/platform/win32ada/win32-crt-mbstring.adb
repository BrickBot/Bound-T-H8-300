--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-crt-mbstring.adb,v $
--  $Revision: 1.1 $ $Date: 2009-08-31 07:20:37 $ $Author: niklas $
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


package body Win32.crt.Mbstring is

   function mbccmp (cpc1, cpc2 : Win32.PCBYTE)
                   return Win32.INT is
   begin
      return mbsncmp (cpc1, cpc2, 1);
   end mbccmp;

end Win32.crt.Mbstring;


