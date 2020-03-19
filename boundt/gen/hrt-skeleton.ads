-- HRT.Skeleton (Decl)
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 19:36:50 $
--
-- $Log: hrt-skeleton.ads,v $
-- Revision 1.5  2015/10/24 19:36:50  niklas
-- Moved to free licence.
--
-- Revision 1.4  2003-03-11 08:23:57  holsti
-- Split execution-bounds stuff from the Programs package to
-- make the child package Programs.Execution.
--
-- Revision 1.3  2001/12/10 15:18:17  holsti
-- The Generate operation takes an assertion-set as parameter.
--
-- Revision 1.2  2001/04/06 11:08:56  ville
-- Deleted unnecessary with clauses
--
-- Revision 1.1  2001/03/14 14:49:50  ville
-- HRT rework second phase: skeleton package splitted
--
-- Revision 1.3  2001/03/13 12:43:43  ville
-- HRT rework first phase: WCP for a call
--
-- Revision 1.2  2000/08/11 12:54:35  saarinen
-- First implementation.
--


with Assertions;
with Programs;
with Programs.Execution;


package HRT.Skeleton is
--
-- Generating the HRT Execution Skeleton File for an HRT program,
-- including WCET analysis of the threads and Protected Objects.
-- The HRT structure is defined in an input file, called the
-- Tasks and Protected Objects file (TPOF).
--


   -- PROVIDED OPERATIONS:


   procedure Generate (
      TPOF_Name  : in     String;
      Program    : in out Programs.Program_T;
      Asserts    : in out Assertions.Assertion_Set_T;
      Bounds_Set :    out Programs.Execution.Bounds_Set_T);
   --
   -- Using :
   --    name of TPOF,
   --    access handles to target program,
   --    assertion set
   -- Giving:
   --    execution bounds for the threads and protected operations,
   --    WCET results on standard output,
   --    Execution Skeleton File.
   
   --
   -- Exceptions.


   TPOF_Syntax_Error : exception;

   TPOF_Error        : exception;

   Path_Branch       : exception;
   
   Path_Broken       : exception;

   Return_Reached    : exception;

   Internal_Error    : exception;

   ESF_Error         : exception;


end HRT.Skeleton;
