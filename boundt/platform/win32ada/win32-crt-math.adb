--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-crt-math.adb,v $
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

package body Win32.crt.Math is

   type AD is access Win32.DOUBLE;

   package HUGE_Constant is
      huge_addr : constant AD;
      pragma Import (C, huge_addr, "__imp__HUGE");
   end HUGE_Constant;

   function HUGE return Win32.DOUBLE is
   begin
      return HUGE_Constant.huge_addr.all;
   end HUGE;

end Win32.crt.Math;
