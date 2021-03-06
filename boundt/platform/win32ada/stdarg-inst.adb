--  $Source: /home/fs/boundt/cvs/boundt/platform/win32ada/stdarg-inst.adb,v $
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

package body Stdarg.Inst is

   function "&" (Args : ArgList; Arg : Interfaces.C.char_array)
                return ArgList is
      type CP is access constant Interfaces.C.char;
      function "&" is new Stdarg.Concat (CP);
   begin
      return Args & Arg (0)'Unchecked_Access;
   end "&";

end Stdarg.Inst;

