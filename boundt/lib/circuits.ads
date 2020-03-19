-- Circuits (decl)
--
-- Tools for building and circuits of clocked, synchronous logic
-- and storage elements, and simulating their execution.
--
-- This package supports the construction of cycle-accurate or
-- cycle-safe simulators of processor parts. It does not directly
-- support static worst-case analysis, but the simulators built
-- with this package may be the starting point for abstracted models
-- of the simulated processor parts, and these abstracted models
-- may be useful static worst-case analysis.
--
-- Author: Niklas Holsti.
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
-- $Date: 2015/10/24 20:53:51 $
--
-- $Log: circuits.ads,v $
-- Revision 1.2  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-06-11 07:58:36  niklas
-- First version.
--


with Ada.Finalization;


package Circuits is


   -- A "circuit" is a collection of connected "parts".
   --
   -- A "part" combines a computation (logical function) with
   -- clock synchronization and latching of the results of the
   -- computation. The output latch may have one or more shift
   -- register stages by which the output of the computation is
   -- delayed for some number of clock cycles, in a pipelined
   -- fashion. Every clock cycle may perform a new computation
   -- and enter its results in the output pipeline, for later
   -- presentation as the output from the part.
   --
   -- Each part has an output that presents a value of some type,
   -- such as Boolean, Natural, or some application-specific type.
   -- The output pipeline can be "tapped" at any stage, giving
   -- outputs with any selected latency from the inputs.
   --
   -- The inputs of a part are not explicitly declared; rather,
   -- the part's computation function uses whatever it needs.
   -- This usually includes outputs of other parts (from some
   -- earlier clock cycle), but may also include the output
   -- from the part itself, or the output of a part from some
   -- other circuit, or some other data, not from a part.
   --
   -- Every "part" belongs to one "circuit". The only function of
   -- the circuit is to provide a "cycle" operation that first
   -- executes the computation belonging to each part and then
   -- sends a clock signal to all parts to latch the result of
   -- the computation into the part's output pipeline.


   type Circuit_T is limited private;
   --
   -- A collection of parts, synchronized to the same clock.


   type Circuit_Ref is access all Circuit_T;
   --
   -- A reference to a Circuit (not necessarily heap-allocated).


   function Cycle (Circuit : Circuit_T) return Natural;
   --
   -- The number of cycles executed in the circuit so far.


   type Root_Part_T (Circuit : Circuit_Ref);
   --
   -- See more below.


   type Part_Ref is access all Root_Part_T'Class;
   --
   -- A reference to some kind of part.


   type Root_Part_T (Circuit : Circuit_Ref)
   is abstract new Ada.Finalization.Limited_Controlled
   with record
      Number : Positive;
   end record;
   --
   -- A common root for all types of parts.
   -- The Circuit is the circuit that contains the part.


   -- overriding
   procedure Initialize (Part : in out Root_Part_T);


   procedure Compute (Part : in out Root_Part_T)
   is abstract;
   --
   -- Executes the computation assigned to the Part.
   -- The computation uses the inputs of the Part to compute
   -- the next output for the Part. The output is stored
   -- internally until the next Clock cycle.


   procedure Clock (Part : in out Root_Part_T)
   is abstract;
   --
   -- Signals the Part to clock the computed result into the
   -- output pipeline of the Part.


   procedure Run_Cycle (Circuit : in out Circuit_T);
   --
   -- Runs the Circuit for one clock cycle.
   -- First, invokes Compute for all parts in the Circuit.
   -- Then, invokes Clock for all parts in the Circuit.
   -- Finally, increments the Cycle count of the Circuit.


   --
   --    Parts that compute various types of values
   --


   generic

      type Value_Type is private;
      -- The kind of values that these parts can compute and output.

      Null_Value : in Value_Type;
      -- The value resulting from a circuit reset (initial value).

      Max_Latency : in Natural := 20;

   package Parts
   is

      subtype Latency_T is Natural range 0 .. Max_Latency;
      --
      -- The available output latencies. Zero means the value
      -- computed before and latched by the last Clock signal.


      type Computer_T is access procedure (
         Circuit : in     Circuit_T;
         Load    :    out Boolean;
         Value   :    out Value_Type);
      --
      -- A procedure that computes a new Value for the part,
      -- possibly using some properties of the Circuit that
      -- contains the part. The procedure also determines
      -- whether the part should Load the new Value on the
      -- next clock cycle. If Load is returned as False, the
      -- state of the part is unchanged on the next clock cycle,
      -- including the full output pipeline, and the Value is
      -- irrelevant.


      type Part_T (
         Computer : Computer_T;
         Circuit  : Circuit_Ref)
      is new Root_Part_T with private;
      --
      -- A part that can compute Values, initially presenting
      -- the Null_Value, and with an output pipeline of (up to)
      -- Max_Latency stages.
      --
      -- The Computer computes the value of the part, and the
      -- part belongs to the given Circuit.


      -- overriding
      procedure Compute (Part : in out Part_T);


      -- overriding
      procedure Clock (Part : in out Part_T);


      function Output (
         From    : Part_T;
         Latency : Latency_T := 0)
      return Value_Type;
      --
      -- The output at the given Latency.


      procedure Load (
         Value : in     Value_Type;
         Into  : in out Part_T);
      --
      -- Loads the Value Into the given part, as if the part
      -- were clocked with the Value as the computed value and
      -- loading enabled. No effect on any other part.


   private


      type Pipeline_T is array (Latency_T) of Value_Type;
      --
      -- The output pipeline (shift register).


      type Part_T (
         Computer : Computer_T;
         Circuit  : Circuit_Ref)
      is new Root_Part_T (Circuit) with
      record
         Last     : Value_Type := Null_Value;
         Load     : Boolean    := False;
         Pipeline : Pipeline_T := (others => Null_Value);
      end record;
      --
      -- Load is the Load flag last Computed.
      -- Last is the Value last Computed. May be undefined
      -- if Load is False.
      -- Pipeline is, of course, the output pipeline.

   end Parts;


private


   type Part_List_T is array (1 .. 200) of Part_Ref;
   --
   -- For holding the parts in a circuit.


   type Circuit_T is record
      Cycle     : Natural := 0;
      Num_Parts : Natural := 0;
      Part      : Part_List_T;
   end record;
   --
   -- The parts in the circuit are Part(1 .. Num_Parts).


end Circuits;
