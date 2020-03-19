-- ALF (decl)
--
-- Interfaces between Bound-T and the ALF language for representing
-- programs, where ALF means ARTIST2 Language for Flow analysis and
-- is defined by the Mälardalen Centre for Real-Time Systems for use
-- with their SWEET tools.
--
-- This is the dummy/null version of this package, for use when the
-- the ALF interface is omitted.
--
-- Author: Niklas Holsti, Tidorum Ltd
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
-- $Date: 2015/10/24 21:11:59 $
--
-- $Log: alf.ads,v $
-- Revision 1.3  2015/10/24 21:11:59  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.2  2013-02-02 11:03:25  niklas
-- BT-CH-0237: ALF evolutions.
--
-- Revision 1.1  2011-09-10 07:38:53  niklas
-- First version.
--


with Programs;
with Programs.Execution;


package ALF is


   Included : constant Boolean := False;
   --
   -- Whether the ALF option is included in the running Bound-T.


   procedure Finish_Options (Program : in Programs.Program_T)
   is null;


   procedure Export_Program (Bounds_Set : in Programs.Execution.Bounds_Set_T)
   is null;


end ALF;
