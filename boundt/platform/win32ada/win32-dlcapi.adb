--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/win32-dlcapi.adb,v $
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


package body Win32.Dlcapi is

   function LLC_XMIT_BUFFER_SIZE return Win32.Size_T is
      --  dlcapi.h:219
   begin
      return Win32.Size_T (LLC_XMIT_BUFFER'Size);
   end LLC_XMIT_BUFFER_SIZE;

end Win32.Dlcapi;


