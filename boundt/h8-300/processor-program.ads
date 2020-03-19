-- Processor.Program (decl)
--
-- Target-specific information attached to the (model of) the target program
-- and the (models of) target subprograms. This implementation is for the
-- Reneseas H8/300 target processor.
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd
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
-- $Revision: 1.7 $
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-program.ads,v $
-- Revision 1.7  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.6  2008/04/29 20:13:52  niklas
-- Updated for BT-CH-0122.
--
-- Revision 1.5  2006/11/05 21:19:37  niklas
-- BT-CH-0036: Property BCC_Signed.
--
-- Revision 1.4  2006/08/23 19:32:42  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.3  2006/08/22 11:10:11  niklas
-- Per BT-CH-0021 moved the Root_Info function from Processor.Program
-- to Processor.Properties and added parameters.
--
-- Revision 1.2  2005/03/17 07:15:18  niklas
-- Changed Info_T to an access (reference) type because it is
-- no longer a visible component of Program_T but must be used
-- via a query function.
--
-- Revision 1.1  2004/06/16 07:41:39  niklas
-- First version.
--


with Format;


package Processor.Program is


   --
   --    Attaching target-specific information to programs
   --


   type Info_T is access Format.Object_T;
   --
   -- The type attached to Programs.Program_T is a reference to
   -- Format.Object_T, providing access to the executable file.


   --
   --    Attaching target-specific information to subprograms
   --


   type Sub_Info_T is record
      Signed_Cond_Unknown : Boolean;
   end record;
   --
   -- Target-specific information attached to a subprogram.
   --
   -- Signed_Cond_Unknown
   --    Whether signed branch conditions should be modelled
   --    as unknown conditions for instructions in this subprogram.
   --    Setting the property BCC_Signed to 1 for a subprogram
   --    turns this flag on for that subprogram.


   Initial_Sub_Info : constant Sub_Info_T := (
      Signed_Cond_Unknown => False);
   --
   -- Initial state to be assigned to new subprograms.


end Processor.Program;
