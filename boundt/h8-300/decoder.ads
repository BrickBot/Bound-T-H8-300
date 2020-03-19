-- Decoder (decl)
--
-- Program decoder for the Renesas H8/300 processor architecture.
--
-- Authors:
--    Samuel Petersson, Mälardalen University
--    Niklas Holsti, Tidorum Ltd.
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
-- $Revision: 1.18 $
-- $Date: 2015/10/26 22:19:13 $
--
-- $Log: decoder.ads,v $
-- Revision 1.18  2015/10/26 22:19:13  niklas
-- Version 4b, and moved to free open-source licence.
--
-- Revision 1.17  2015/10/26 21:57:56  niklas
-- Updated to use current Options services, etc, and to omit Extra_Symbols.
--
-- Revision 1.16  2009/12/02 14:20:01  niklas
-- BT-CH-0188: H8/300 updates for bit-widths and Word_T.
--
-- Revision 1.15  2009-07-15 11:23:13  niklas
-- Updated Decoded.Decoded_Instruction to take a Program_T
-- instead of a Model_Ref, per BT-CH-0158.
--
-- Revision 1.14  2008-10-29 14:48:37  niklas
-- Updated for BT-CH-0142: Integrate_Call.Cond.
--
-- Revision 1.13  2008/04/29 20:12:47  niklas
-- Updated for BT-CH-0121 by removing param Decode.Decoded_Step.
--
-- Revision 1.12  2007/12/27 23:12:33  niklas
-- BT-CH-0104: S-record program format for Bound-T/H8-300.
--
-- Revision 1.11  2007/03/22 18:51:35  niklas
-- BT-CH-0053.
--
-- Revision 1.10  2007/01/21 22:07:59  niklas
-- Updated for BT-CH-0041.
--
-- Revision 1.9  2006/11/28 19:23:44  niklas
-- Updated for BT-CH-0037.
--
-- Revision 1.8  2006/08/23 19:32:42  niklas
-- Updates for BT-CH-0025.
--
-- Revision 1.7  2006/08/22 11:13:47  niklas
-- Updated for recent changes in generic/specific interface by
-- renaming Loading_Error to Decoder_Error and adding Patch_Code.
--
-- Revision 1.6  2005/09/05 11:32:30  niklas
-- Updated for BT-CH-0007.
-- Made the function Calling_Protocol visible to child packages.
--
-- Revision 1.5  2005/09/03 11:52:37  niklas
-- BT-CH-0006.
--
-- Revision 1.4  2005/03/22 20:48:26  niklas
-- Removed unnecessary context clauses.
--
-- Revision 1.3  2005/03/17 07:28:32  niklas
-- Updated for changes in the general part as follows.
-- Parameters of a reference type use "in" mode.
-- Removed all Resolve_Xxx operations.
-- Removed all operations for calling protocols.
-- Moved some stuff to the new package Processor.Properties.
-- Added the Post_Analysis procedure.
--
-- Revision 1.2  2004/06/27 15:49:11  niklas
-- Added Start and Stop procedures.
--
-- Revision 1.1  2004/06/16 07:41:36  niklas
-- First version.
--


with Arithmetic;
with Assertions;
with Calling;
with Flow;
with Flow.Origins;
with ILP;
with Processor;
with Programs;
with Programs.Execution;


package Decoder is
--
-- Decoding of the target processor's instructions into flow graphs
-- with data-flow effects and branch predicates.
--
-- This package is highly target-dependent.
--


   Decoder_Error : exception;
   --
   -- Raised for any unrecoverable error in accessing or decoding
   -- the target program executable file.
   -- When the exception is raised, the error has already been
   -- reported on standard-error.


   --
   ---   Loading the target program for analysis
   --


   procedure Initialize (
      File_Name : in     String;
      Program   : in out Programs.Program_T);
   --
   -- Reads the target program from the program file with the given
   -- File_Name. The file is a compiled, linked, binary, executable
   -- file containing an H8/300 program.
   --
   -- The operation loads the program instructions and constant data
   -- into an in-memory model of the load image.
   --
   -- The operation also loads the symbolic information (debug info,
   -- symbol table) into the Bound-T symbol-table structure referenced
   -- by Program.Symbol_Table.
   --
   -- May raise Decoder_Error.


   procedure Initialize (
      Subprogram : in Programs.Subprogram_T;
      Within     : in Programs.Program_T);
   --
   -- Initializes the (newly created) Subprogram, Within the
   -- given program, for any H8/300-specific attributes.
   --
   -- This operation is called before any call to the Subprogram
   -- is created and before any decoding of the Subprogram.


   procedure Patch_Code (
      Program : in     Programs.Program_T;
      Address : in     Processor.Code_Address_T;
      Data    : in     String;
      Params  : in     Programs.Code_Address_List_T;
      Valid   :    out Boolean);
   --
   -- Patches the loaded code of the Program, starting from the
   -- given Address, using the given Data and Params, and returns
   -- Valid as True for success and False for error.
   --
   -- The Program, as given, has been Initialized and may already
   -- have been subjected to other Patch_Code calls.
   --
   -- The format of the Data string and the meaning and use of the
   -- Params depend on the target processor.


   procedure Dump_Program (File_Name : in String);
   --
   -- Opens and reads the target-program executable file identified by
   -- the file-name, and describes its contents on standard output.
   --
   -- This operation is invoked when Bound-T is run on a target program
   -- but without naming any subprograms to be analyzed.


   procedure Pre_Analysis (
      Bounds : in Programs.Execution.Bounds_Set_T);
   --
   -- Any sort of preprocessing or initial processing that may be needed
   -- for the program under analysis, represented by the set of execution
   -- Bounds and in particular by the set of root calls of the Bounds.
   --
   -- This operation is entirely processor-specific. It is invoked once,
   -- after the command-line options have been initialized and the
   -- target program has been Initialized and possibly patched, and
   -- after the assertions, if any, have been parsed and applied to
   -- the target program (at least the assertions that specify target-
   -- specific subprogram properties).
   --
   -- The operation can add root calls and modify subprogram properties.
   --
   -- After this operation finishes, the actual analysis starts to Decode
   -- the root subprograms.


   --
   ---   Decoding target instructions and building control-flow graphs
   --


   procedure Start (
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Resuming   : in Boolean);
   --
   -- Signals the start of the decoding of a given Subprogram within
   -- a given Program. This may be initial start, or we may be Resuming
   -- the decoding after some dynamic flow was resolved.
   --
   -- The next calls of the Decode procedure (below) will all refer to
   -- this Subprogram and this Program, until the Stop operation is
   -- called to signal stop decoding.
   --
   -- The Start/Stop operations are for information only; they do not
   -- have to do anything.


   procedure Decode (
      Loose_Edge : in Flow.Loose_Edge_T;
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Graph      : in Flow.Graph_T);
   --
   -- Decodes the instruction that is the target of the given Loose_Edge
   -- in the given Program and the given Subprogram. Adds the resulting
   -- decoded step to the control-flow Graph under construction for this
   -- Subprogram. Possibly adds other new steps, edges and loose edges
   -- to the Graph. May add new calls to new or old subprograms to the
   -- Program.
   --
   -- The operation can choose to decode as many new steps as it pleases,
   -- but at least the step at the target Step Address of the given
   -- Loose_Edge must be decoded and added to the graph.
   --
   -- For other steps, the operation can choose between the following
   -- methods, as convenient:
   --
   --   > Decode the step in the same call and add it to the graph,
   --     adding edges as needed.
   --
   --   > Add a new, loose edge with a Target identifying the
   --     step-address of the step, and wait for a new call to
   --     handle this loose edge (in the Loose_Edge parameter).
   --
   -- The same two choices apply to any edge that the operation
   -- creates: the edge can be added in a bound form, provided that
   -- both its source and target steps exist, or using just the
   -- source step and the target step-address. If the latter gives
   -- a loose edge, the control-flow analyser will later call for
   -- the decoding of the unbound loose edge.


   procedure Integrate_Call (
      Host   : in Programs.Subprogram_T;
      Graph  : in Flow.Graph_T;
      Callee : in Programs.Subprogram_T;
      Source : in Flow.Step_T;
      Cond   : in Arithmetic.Condition_T;
      Time   : in Processor.Time_T;
      Target : in Flow.Step_Tag_T);
   --
   -- Extends the flow Graph of the Host subprogram to include an
   -- integrated call to a given Callee. The integrated call should
   -- be represented by an edge from the Source step (in the Graph)
   -- to the Target step-tag (modelling the first instruction in the
   -- Callee). This edge has the given pre-Condition and may need
   -- some execution Time.
   --
   -- This operation is called from Flow.Calls.Add_Call for a call
   -- that shall be integrated.


   procedure Integrate_Return (
      Host   : in Programs.Subprogram_T;
      Graph  : in Flow.Graph_T;
      Source : in Flow.Step_T;
      Time   : in Processor.Time_T;
      Target : in Flow.Step_Tag_T);
   --
   -- Extends the flow-Graph of the Host subprogram to include a return
   -- from an integrated call (to some callee not explicitly given).
   -- The return should be represented by an edge from the Source step
   -- (in the Graph) to the Target step-tag (modelling the return point
   -- for the integrated call). This edge many need some execution Time.
   --
   -- This operation is called from Flow.Calls.Return_After when the
   -- call (that returns) is an integrated one.


   procedure Stop (
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Graph      : in Flow.Graph_T;
      Suspended  : in Boolean);
   --
   -- Signals the end of the decoding of a given Subprogram within
   -- a given Program, resulting in given flow Graph. This may be the
   -- final stop, or decoding may only be Suspended temporarily for
   -- resolving dynamic flow and will then be resumed again.
   --
   -- There will be no more calls of the Decode procedure (above) before
   -- the next call of the Start procedure.
   --
   -- The Start/Stop operations are for information only; they do not
   -- have to do anything.


   procedure Stub (
      Entry_Tag  : in Flow.Step_Tag_T;
      Graph      : in Flow.Graph_T;
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T);
   --
   -- Creates a dummy or stub control-flow Graph to represent a Subprogram
   -- that is called but will not be analyzed, usually because the WCET
   -- and perhaps other properties of this subprogram are known from
   -- user assertions. As a result, the subprogram will not be decoded,
   -- and the stub flow-graph bears no relation to the real flow-graph;
   -- it is a synthetic graph created to match the assertions.
   --
   -- The Graph parameter is initially the empty graph. The returned
   -- stub Graph must contain at least one step, which is usually
   -- given the Entry_Tag step-tag, which corresponds to the entry address
   -- of the Subprogram.
   --
   -- This operation may update the Program and Subprogram, but this
   -- is not usually necessary.


   procedure Finish_Arithmetic (
      Subprogram  : in Programs.Subprogram_T;
      Graph       : in Flow.Graph_T;
      Call_Bounds : in Programs.Execution.Call_Bounds_List_T);
   --
   -- Invoked to possibly finish the arithmetic effects, as created in
   -- the Graph for the given Subprogram, immediately before the first
   -- data-flow analysis of the Graph. At this time, the Graph has no
   -- more loose edges (the last such edge was just Decoded) but may
   -- have unresolved boundable edges and may thus still grow, after
   -- the various data-flow analyses try to resolve such edges. The
   -- Graph is provided with its node (basic-block) structure and a
   -- loop structure. The Call_Bounds parameters gives all the calls
   -- from the Subprogram (so far known) and their execution bounds
   -- on the universal level or in the context of this call from this
   -- Subprogram.
   --
   -- This operation may modify the arithmetic effects and efforts of
   -- Graph steps, and the preconditions and times of Graph edges,
   -- but must not add or remove any steps or edges in the Graph.
   -- The operation must leave the arithmetic effects in a consistent
   -- and complete state: the effect of any step must include the
   -- effect on any cell that the step can modify (when TBA aliasing
   -- is considered).
   --
   -- If later data-flow analysis resolves boundable edges and causes
   -- the flow-graph to grow, Finish_Arithmetic will be called again
   -- on the enlarged flow-graph. The enlarged flow-graph contains the
   -- modifications from the earlier call(s) of Finish_Arithmetic, so
   -- Finish_Arithmetic may have to be careful not to apply them again,
   -- and may have to use some mechanism to tell which steps and edges
   -- are old, which new.
   --
   -- This operation contains only target-specific actions.


   function Value_Origins_Applicable (
      To    : Programs.Execution.Bounds_Ref;
      Along : Programs.Call_Path_T)
   return Boolean;
   --
   -- Whether it would be useful and possible to apply value-origin
   -- analysis To the given execution bounds, created Along the given
   -- call-path, for use of the procedure Apply_Value_Origins, below.


   procedure Apply_Value_Origins (
      Origins : in Flow.Origins.Map_Ref;
      Bounds  : in Programs.Execution.Bounds_Ref);
   --
   -- This operation is invoked when value-origin analysis has found
   -- the Origins of the values of storage cells on entry to each step
   -- in the flow-graph that underlies the given execution Bounds.
   -- The operation is not required to do anything but can use the Origins
   -- to improve or extend the computation model that is associated with
   -- the Bounds.


   procedure Finish (
      Program    : in Programs.Program_T;
      Subprogram : in Programs.Subprogram_T;
      Graph      : in Flow.Graph_T;
      Assert_Map : in Assertions.Assertion_Map_T);
   --
   -- Finishes the creation of a flow Graph for a given Subprogram, within
   -- a given target Program, after all steps and edges are decoded and
   -- dynamism is resolved as far as possible (see the Resolve operations
   -- below). The assertion map for this subprogram is also given.
   --
   -- This operation contains only target-specific actions; on the
   -- generic level, the graph is already completed.
   --
   -- Things that this operation can do include:
   -- > Modifying the information of steps, including the effort.
   -- > Modifying the execution time of step edges.
   --
   -- Things that this operation is not allowed to do include:
   -- > Adding steps or edges to the graph.
   -- > Modifying the effect of a step.
   --
   -- A typical use for this operation is to check for register read/write
   -- interference between successive instructions (steps) and add the
   -- corresponding waiting time (blocked cycles) to the edges between
   -- such instruction pairs.


   --
   ---   Target-specific additional analysis
   --


   procedure Additional_Analysis (
      Program    : in Programs.Program_T;
      Asserts    : in Assertions.Assertion_Set_T;
      Bounds_Set : in Programs.Execution.Bounds_Set_T);
   --
   -- This operation is invoked when all subprograms/calls have
   -- complete execution bounds, such as loop-bounds, but the worst-
   -- case paths and WCETs have not yet been computed.
   --
   -- The operation can modify the program model in any way, as long
   -- as the result is a consistent model (e.g. the flow-graphs are
   -- consistent with the loop structures and with the execution
   -- bound structures).


   function Additional_Variables (
      Subprogram : Programs.Subprogram_T;
      Bounds     : Programs.Execution.Bounds_Ref)
   return ILP.Var_List_T;
   --
   -- Additional integer-valued variables for the ILP (IPCET)
   -- analysis of the worst-case path in the given Subprogram as
   -- bounded by the give Bounds. These variables may then be used
   -- in the Additional_Time and/or in the Additional_Bounds.


   function Additional_Time (
      Subprogram : Programs.Subprogram_T;
      Bounds     : Programs.Execution.Bounds_Ref)
   return ILP.Expression_T;
   --
   -- Additional terms for the expression that gives an upper bound
   -- on the execution time of the given Subprogram, as constrained
   -- by the given Bounds, in the ILP (IPCET) analysis to find the
   -- worst-case path.


   procedure Additional_Bounds (
      Subprogram : in     Programs.Subprogram_T;
      Bounds     : in     Programs.Execution.Bounds_Ref;
      Solver     : in out ILP.Solver_T);
   --
   -- This operation is invoked during during the computation of the
   -- worst-case path and WCET for a given subprogram, by means of
   -- Integer Linear Programming (ILP).
   --
   -- The operation can add any type of ILP bound or constraint to the
   -- ILP formulation of the worst-case path problem, represented by the
   -- parameter Solver.
   --
   -- The operation should not modify the program model in any way.


   procedure Take_Additional_Values (
      Values     : in ILP.Valuation_T;
      Subprogram : in Programs.Subprogram_T;
      Bounds     : in Programs.Execution.Bounds_Ref);
   --
   -- Takes the results from the ILP analysis for the additional
   -- variables and makes some target-specific use of them.
   --
   -- Values
   --    The ILP solution for the target-specific variables
   --    defined by Additional_Variables for these Bounds.
   --    Note that the variables may be listed in a different
   --    order than given by Additional_Variables.
   -- Subprogram, Bounds
   --    The subprogram and execution bounds for which the ILP
   --    solution was computed.
   --
   -- This operation is the only direct user of the values of the
   -- additional variables. The target-independent part of Bound-T does
   -- not make any use of them. The only other effect the additinoal
   -- variables may arise indirectly from their use in Additional_Bounds
   -- (constraining the ILP solution) or in Additional_Time (modifying
   -- the value of the WCET bound).


   procedure Post_Analysis (
      Bounds : in Programs.Execution.Bounds_Set_T);
   --
   -- Any sort of post-processing or further processing of an analyzed
   -- target program. This operation is called after all other analysis
   -- is completed for all the root subprograms, resulting in the
   -- given execution Bounds, which may or may not be fully bounded.


   --
   ---   Graphic decorations
   --


   function Decoded_Instruction (
      Step    : Flow.Step_T;
      Program : Programs.Program_T)
   return String;
   --
   -- An image of the decoded (disassembled) instruction that the
   -- given Step models, under the given computation Model.
   --
   -- This is for "-draw decode".


private


   function Calling_Protocol (Program : Programs.Program_T)
   return Calling.Protocol_Ref;
   --
   -- Creates a calling protocol object suitable for this Program.


end Decoder;
