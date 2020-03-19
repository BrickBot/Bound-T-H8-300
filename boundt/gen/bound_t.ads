-- Bound_T (decl)
--
-- Main package of the Bound-T static program analyser.
--
-- This package provides the Main operation and the top-level handles
-- to the data structures: the target program to be analysed, the
-- assertions that constrain the execution scenario, and the resulting
-- execution bounds.
--
-- Authors: Space Systems Finland, 1999:
--     Niklas Holsti
--     Thomas Langbacka
--     Sami Saarinen
--     Ville Sipinen
--     Mikko Ala-Fossi
--     Eemeli Kuumola
--     Satu Sihvo
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
-- $Date: 2015/10/24 19:36:47 $
--
-- $Log: bound_t.ads,v $
-- Revision 1.2  2015/10/24 19:36:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2005-08-08 13:58:28  niklas
-- First version.
--


with Assertions;
with Programs;
with Programs.Execution;


package Bound_T is


   --
   --    The analysis data
   --
   -- These data are made public so that they can be accessed by
   -- an *optional* graphical user interface (the Analysis Workbench
   -- or AW). The architecture is designed so that AW can be included
   -- or omitted from the executable simply by choosing one of two
   -- main procedures (boundt_<target> or aw_<target).


   Program : Programs.Program_T;
   --
   -- The target program under analysis.

   Asserts : Assertions.Assertion_Set_T;
   --
   -- The assertions given in assertion file.

   Bounds_Set : Programs.Execution.Bounds_Set_T;
   --
   -- The computed execution bounds for the root subprograms
   -- and their callees.      


   procedure Main;
   --
   -- Analyses a target program according to the command-line
   -- options and input files named on the command line.
   --
   -- Emits the results, including warnings and errors, on the
   -- standard output channel and standard error channel. May also
   -- create additional output files with fixed names (as explained in
   -- the user manual) or with names given on the command line.
   --
   -- Leaves the input and results in the variables Program, Asserts
   -- and Bounds_Set.


end Bound_T;
