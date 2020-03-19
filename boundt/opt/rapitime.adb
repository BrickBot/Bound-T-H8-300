-- RapiTime (null body)
--
-- This is the dummy/null version of this package, for use when the
-- RapiTime interface is omitted.
--
-- RapiTime (tm) is a timing analysis tool-set from Rapita Systems Ltd.
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
-- $Revision: 1.5 $
-- $Date: 2015/10/24 21:12:00 $
--
-- $Log: rapitime.adb,v $
-- Revision 1.5  2015/10/24 21:12:00  niklas
-- Moved to free licence for the dummy options.
--
-- Revision 1.4  2012-02-14 20:58:41  niklas
-- Added Finish_Options.Valid, for BT-CH-0229.
--
-- Revision 1.3  2008-08-04 11:19:18  niklas
-- Updated for additions to actual implementation.
-- Removed warning re right to read, since this null version
-- has no confidential information.
--
-- Revision 1.2  2006/05/17 19:45:30  niklas
-- Introduced the RapiTime.Target package (target-specific actions)
-- and therefore renamed Export to Export_Program and removed all
-- other parameters except the Bounds_Set.
--
-- Revision 1.1  2006/04/20 05:48:55  niklas
-- First version of null body.
--


package body RapiTime is


   procedure Finish_Options (
      Program : in     Programs.Program_T;
      Valid   :    out Boolean)
   is
   begin

      Valid := True;

   end Finish_Options;


   function Is_Ipoint (Callee : Processor.Code_Address_T) return Boolean
   is
   begin

      return False;

   end Is_Ipoint;


   procedure Export_Program (Bounds_Set : in Programs.Execution.Bounds_Set_T)
   is
   begin

      null;

   end Export_Program;


end RapiTime;
