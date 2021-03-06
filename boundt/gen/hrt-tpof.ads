-- HRT.TPOF (decl)
--
-- A component of the Bound-T Worst-Case Execution Time Tool.
--
-------------------------------------------------------------------------------
-- Copyright (c) 1999 .. 2015 Tidorum Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders and contributors "as is" and
-- any express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright owner or contributors be liable for
-- any direct, indirect, incidental, special, exemplary, or consequential damages
-- (including, but not limited to, procurement of substitute goods or services;
-- loss of use, data, or profits; or business interruption) however caused and
-- on any theory of liability, whether in contract, strict liability, or tort
-- (including negligence or otherwise) arising in any way out of the use of this
-- software, even if advised of the possibility of such damage.
--
-- Other modules (files) of this software composition should contain their
-- own copyright statements, which may have different copyright and usage
-- conditions. The above conditions apply to this file.
-------------------------------------------------------------------------------
--
-- $Revision: 1.2 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: hrt-tpof.ads,v $
-- Revision 1.2  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.1  2001-03-14 14:51:23  ville
-- HRT rework second phase: skeleton package splitted
--

package HRT.TPOF is

   function Check_TPOF_Name (TPOF : String)
                             return String;
     --
     -- Checks that TPOF name exists and has the correct suffix.
     -- Returns a string to be used as ESF name.

   procedure Get_TPOF
     (TPOF_Name  : in     String;
      Program    : in out Programs.Program_T;
      HRT_Struct :    out HRT_Struct_T);
     --    using :
     --        name of TPO file,
     --        access handles to target program
     --    giving:
     --        HRT structure as defined in TPOF,
     --        or failure (errors in TPOF))

   TPOF_Syntax_Error : exception;

   TPOF_Error        : exception;

end HRT.TPOF;

