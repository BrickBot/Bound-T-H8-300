-- Processor.Properties (decl)
--
-- Processor-specific properties for the Renesas H8/300 processor.
--
-- This package provides
--
-- > Functions to parse strings into code-addresses and storage cells.
-- > Default Property_T values/ranges for subprograms.
-- > Processor Power_T at a given flow node, under given assertions.
-- > Bounds on storage cells on entry to a given subprogram.
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
-- $Revision: 1.11 $
-- $Date: 2015/10/26 22:19:15 $
--
-- $Log: processor-properties.ads,v $
-- Revision 1.11  2015/10/26 22:19:15  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.10  2015/10/26 22:01:05  niklas
-- Updated to use current Options services.
--
-- Revision 1.9  2010/02/03 21:24:18  niklas
-- Added Offset_Return per BT-CH-0216.
--
-- Revision 1.8  2009-07-15 12:02:09  niklas
-- Added Assumed_Net_Change, for BT-CH-0179.
--
-- Revision 1.7  2008-04-29 20:13:52  niklas
-- Updated for BT-CH-0122.
--
-- Revision 1.6  2007/03/22 18:51:35  niklas
-- BT-CH-0053.
--
-- Revision 1.5  2006/10/30 23:09:53  niklas
-- Updated for BT-CH-0033.
--
-- Revision 1.4  2006/08/23 19:32:42  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.3  2006/08/22 11:10:12  niklas
-- Per BT-CH-0021 moved the Root_Info function from Processor.Program
-- to Processor.Properties and added parameters.
--
-- Revision 1.2  2005/03/22 20:38:49  niklas
-- Defined the constant Entry_Stack_Height for use in stacked
-- parameter mapping as well as in Entry_Bounds.
--
-- Revision 1.1  2005/03/17 07:12:50  niklas
-- First version.
--


with Assertions;
with Flow;
with Processor.Program;
with Programs;
with Storage;
with Storage.Bounds;


package Processor.Properties is


   Address_Error : exception;
   --
   -- Raised from Subprogram_Address and Variable_Cell for any
   -- Address string that is not valid for this target.


   function Subprogram_Address (Sub_Address : String)
   return Code_Address_T;
   --
   -- Given a string that contains an external (textual) representation
   -- of a subprogram's entry address, returns the address in internal form.
   --
   -- Raises Address_Error if the given string is not in the correct
   -- (target-dependent) syntactical form.
   --
   -- This function is used from the assertion parser and also from
   -- the command-line parser, when a subprogram (root subprogram for
   -- the command line) is identified by an address string.


   function Code_Offset (Offset : String)
   return Code_Offset_T;
   --
   -- Given a string that contains an external (textual) representation
   -- of a code-address offset, returns the offset in internal form.
   --
   -- Raises Address_Error if the given string is not in the correct
   -- (target-dependent) syntactical form.
   --
   -- This function is used from the assertion parser.


   function Offset_Image (From, To : Code_Address_T) return String;
   --
   -- The offset From a given code address To another code address,
   -- in a form suitable for the Code_Offset function.
   --
   -- This function is used to display the location of loops in terms
   -- of the offset of the loop-head from the start of the containing
   -- subprogram.


   function Offset_Return (
      From : Flow_State_T;
      By   : Code_Offset_T)
   return Flow_State_T;
   --
   -- The offset return point (flow state) of a subprogram call where
   -- the callee is known to return to a point that is offset By a
   -- known amount From the normal return point offered by the caller.


   function Variable_Cell (Var_Address : String)
   return Storage.Cell_T;
   --
   -- Given a string that contains an external (textual) representation
   -- of a cell (variable, parameter, register, ...), returns the
   -- corresponding internal cell. If necessary, the cell is created and
   -- added to the cell-store.
   --
   -- Raises Address_Error if the given string is not in the correct
   -- (target-dependent) syntactical form.


   function Blank_Info (
      Address : Code_Address_T;
      Program : Programs.Program_T)
   return Processor.Program.Sub_Info_T;
   --
   -- The initial processor-specific information for the subprogram
   -- with the given entry Address within the given Program.
   --
   -- This subprogram is newly identified as a root subprogram or in
   -- an assertion. For root subprograms, the procedure Define_As_Root
   -- can later alter the information. This function is not used for
   -- subprograms discovered (for the first time) as callees, because
   -- the info is a parameter to Programs.Identify_Callee.


   procedure Define_As_Root (
      Subprogram : in     Programs.Subprogram_T;
      Info       : in out Processor.Program.Sub_Info_T);
   --
   -- Defines processor-specific information for a Subprogram that
   -- is identified as a root subprogram and is thus to be analysed
   -- even if never called by any other subprogram in the analysis.
   --
   -- This operation is called after the subprogram has been created
   -- with the Blank_Info (see above), and after applying all
   -- asserted properties and subprogram options to the Subprogram,
   -- using the procedure Apply (see below).


   procedure Inform_By_Call (
      Caller      : in     Programs.Subprogram_T;
      Callee      : in     Programs.Subprogram_T;
      Call_Info   : in     Processor.Program.Sub_Info_T;
      Callee_Info : in out Processor.Program.Sub_Info_T);
   --
   -- Combines processor-specific information (Call_Info), for a
   -- call from a Caller subprogram to a Callee subprogram, with
   -- the information already known for the Callee (Callee_Info) and
   -- updates the latter. Usually this also includes some level of
   -- checks that the calling sequence (as represented in Call_Info)
   -- is coherent with the properties of the Callee (as represented
   -- in Callee_Info).


   function Default_Properties (
      Subprogram : Programs.Subprogram_T)
   return Assertions.Property_Table_T;
   --
   -- Default bounds on processor-specific properties that apply
   -- to a given subprogram, before any user-given assertions
   -- are applied.
   --
   -- These defaults may, but need not, depend on the subprogram.
   -- Global-level property-assertions override these defaults
   -- in any case, as do subprogram-level assertions.


   function Power (
      Within : Flow.Node_T;
      Assert : Assertions.Assertion_Map_T)
   return Power_T;
   --
   -- The processor-power within the given node, which may depend
   -- on the node itself and on the assertions covering the node.


   procedure Apply (
      Property : in Property_T;
      Values   : in Storage.Bounds.Interval_T;
      To       : in Programs.Subprogram_T);
   --
   -- Applies the assertion that the value of the Property is in
   -- the given interval of Values, To the given subprogram.
   --
   -- The effect, if any, is usually a change in the processor-
   -- specific info (Processor.Programs.Sub_Info_T) of the subprogram.


   function Assumed_Net_Change (
      Stack : Programs.Stack_T;
      Stub  : Programs.Subprogram_T)
   return Storage.Bounds.Limit_T;
   --
   -- The assumed net change in the (local) stack height of this Stack
   -- over any call of this Stub subprogram which is not an analysed
   -- subprogram. That is, the subprogram is represented by a stub
   -- and is omitted from the analysis.
   --
   -- This function is applied to subprograms that are omitted from
   -- the analysis because of an "omit" assertion or an "-alone"
   -- option and which do not have an asserted final stack height
   -- for this Stack. Assuming a fixed net change for calls of such
   -- stubs often helps the analysis of the (non-stub) caller.
   --
   -- The function should return Unlimited if no assumption is
   -- desirable.


   function Entry_State (Subprogram : Programs.Subprogram_T)
   return Flow_State_T;
   --
   -- The flow-state on entry to the given Subprogram.
   --
   -- This function is used to initialise the control-flow analysis
   -- of the Subprogram. It becomes the flow-state for the entry step
   -- in the Subprogram's control-flow graph.


   function Entry_Bounds (
      Subprogram : Programs.Subprogram_T;
      Sub_Info   : Processor.Program.Sub_Info_T)
   return Storage.Bounds.Cell_Interval_List_T;
   --
   -- Returns a list of initial bounds known for a
   -- Subprogram with the given Sub_Info.


end Processor.Properties;
