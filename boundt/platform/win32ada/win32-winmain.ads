--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-winmain.ads,v $
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

with Win32.Windef;
package Win32.Winmain is

   --  **************************************************************
   --  Functions which grab the parameters passed to WinMain by NT.
   --  See also the Ada.Command_Line package, Ada 95 Reference Manual
   --  section A.15.
   --
   --  Note:  There is one body for GNAT and one body for AdaMagic.
   --  **************************************************************

   function Get_hInstance return Win32.Windef.HINSTANCE;

   function Get_hPrevInstance return Win32.Windef.HINSTANCE;

   function Get_lpCmdline return Win32.PCSTR;

   function Get_nCmdShow return Win32.INT;

end Win32.Winmain;


