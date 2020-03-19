-- BoundT_H8_300 (decl and body)
--
-- Main procedure for the Bound-T static program analyser for the
-- Renesas H8/300 target processor architecture.
--
-- This main procedure binds to a pure command-line tool and omits the
-- Analysis Workbench graphical interface.
--
-- Author: Niklas Holsti
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
-- $Revision: 1.3 $
-- $Date: 2015/10/26 22:19:12 $
--
-- $Log: boundt_h8_300.adb,v $
-- Revision 1.3  2015/10/26 22:19:12  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.2  2005/08/24 10:31:58  niklas
-- Updated to use Bound_T.Main instead of WCET_Tool.Main and
-- to explain that this is the main procedure for the command-
-- line tool, as distinct from the Analysis Workbench.
--
-- Revision 1.1  2004/06/16 07:41:34  niklas
-- First version.
--


with Bound_T;


procedure BoundT_H8_300
is
begin

   Bound_T.Main;

end BoundT_H8_300;
