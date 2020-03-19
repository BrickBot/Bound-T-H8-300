-- Calling (decl)
--
-- Calling protocols and related stuff.
--
-- Calling protocols are target-dependent, and may also depend on
-- the programming language or operating system used on the target.
-- The same target program may even use several calling protocols.
--
-- A calling protocol has the following roles in Bound-T:
--
-- > The protocol defines a mapping of callee-framed cells to
--   caller-framed cells, for passing call-context to the analysis
--   of the callee, and effects of the call back to the caller.
--   The mapping may be static, or depend on the values of specific
--   "framing" cells; these cells are defined by the calling protocol.
--
-- This is the main purpose of the calling protocol. Other Cell_T
-- values in the caller and callee can be fixed cells (refer to
-- the same location in the caller and the callee) or private
-- framed cells of the callee (not visible to the caller) or vice
-- versa (private cells of the caller, not visible to the callee).

-- > The protocol defines a predicate to say which caller-framed
--   cells can be modified by a call that has an unknown effect,
--   for example a call to a "stub" subprogram where the real callee
--   is not analysed at all. Again, this predicate can be static or
--   can depend on "framing" cells.
--
-- > The protocol may assert that certain cells are invariant across
--   the call, even if the callee contains assignments to these cells.
--
-- These cells are typically callee-save registers, which are thus
-- both read and written by the callee, for example by push and pop
-- instructions.
--
-- > The protocol may assert that certain cells are invariant as
--   above, and are also irrelevant for bounding.
--
-- For example, this could hold for the stack pointer, since the
-- callee should leave it invariant, and the absolute value of the
-- stack pointer should not influence the execution of the callee.
--
-- As defined in programming systems, calling protocols often define
-- cells that are volatile (become undefined) as the result of a
-- call. In Bound-T, such cells are usually detected automatically
-- as output cells of the callee. However, when the callee is a stub
-- and the real callee is not analysed the concept of volatile or
-- non-volatile cells is important for constraining the possible
-- effect of the call on the caller's computation.
--
-- Calling protocols are implemented in Bound-T as processor-specific
-- "boundable" types (derived from Storage.Bounds.Boundable_T). The
-- present package derives an abstract calling-protocol type to act as
-- the root of all calling-protocol types.
--
-- The present package also defines types to store parameter maps.
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
-- $Revision: 1.8 $
-- $Date: 2015/10/24 19:36:48 $
--
-- $Log: calling.ads,v $
-- Revision 1.8  2015/10/24 19:36:48  niklas
-- Moved to free licence.
--
-- Revision 1.7  2010-01-30 21:13:29  niklas
-- BT-CH-0216: Subprograms have a Return_Method_T attribute.
--
-- Revision 1.6  2007-03-18 12:50:37  niklas
-- BT-CH-0050.
--
-- Revision 1.5  2007/01/13 13:51:03  niklas
-- BT-CH-0041.
--
-- Revision 1.4  2006/02/27 09:15:52  niklas
-- Added Static_Protocol_T.
--
-- Revision 1.3  2005/02/23 09:05:17  niklas
-- BT-CH-0005.
--
-- Revision 1.2  2005/02/19 20:34:56  niklas
-- BT-CH-0003.
--
-- Revision 1.1  2005/02/16 21:11:42  niklas
-- BT-CH-0002.
--


with Processor;
with Storage;
with Storage.Bounds;


package Calling is


   --
   ---   Stub levels
   --


   subtype Stub_Level_T is Natural;
   --
   -- The "stub level" of a subprogram is the length of the shortest
   -- feasible call-path from the given subprogram to a stub subprogram,
   -- thus:
   -- > zero (0) if the subprogram is itself a stub,
   -- > one  (1) if the subprogram calls a stub subprogram,
   -- > two  (2) if the subprogram calls a subprogram that
   --            has stub level 1 (that is, the callee directly
   --            calls a stub),
   -- > and so on, until
   -- > Stub_Level_T'Last if the Subprogram is not itself a stub
   --   nor calls any stub, directly or indirectly.
   --
   -- Unresolved dynamic calls are counted as calls to stubs, because
   -- the arithmetic effect of the unknown callee is unknown.
   --
   -- Because the definition of stub level only includes feasible calls,
   -- the stub level is really a property of some "execution bounds" for
   -- the subprogram, not directly a property of the subprogram.


   Calls_No_Stub : constant Stub_Level_T := Stub_Level_T'Last;
   --
   -- A stub-level that indicates that the subprogram is not a stub
   -- and calls no stub, directly or indirectly.


   --
   ---   Parameter maps
   --


   type Map_Kind_T is (
      Fixed,
      Static,
      Dynamic,
      Privy);
   --
   -- The ways in which a callee cell can be associated with a
   -- caller cell and share the same data location:
   --
   -- Fixed
   --    The same Cell_T value is applicable in the caller and
   --    the callee. No mapping is needed.
   --
   -- Static
   --    A different Cell_T value is used in the caller and
   --    the callee for the same data location. The caller's
   --    Cell_T value can be determined entirely statically
   --    from the callee's Cell_T and the call.
   --
   -- Dynamic
   --    A different Cell_T value is used in the caller and
   --    the callee for the same data location. To determine
   --    the caller's Cell_T for a given callee Cell_T, we need
   --    the values of the "framing cells" of the call.
   --
   -- Privy
   --    The data-location is accessible in the callee only.
   --    Typically, it ceases to exist when the callee returns.
   --    All cells that model the local stack height of some
   --    stack should be classed as Privy cells because each
   --    subprogram (activation) has its own local stack height.


   subtype Mapped_Kind_T is Map_Kind_T range Static .. Dynamic;
   --
   -- The values of Map_Kind_T that mean that the same data
   -- location is described by different Cell_T values in
   -- the caller and the callee.


   --
   ---   Calling protocols
   --


   type Protocol_T is abstract new Storage.Bounds.Boundable_T
   with null record;
   --
   -- A calling protocol, probably instantiated (specialized, parametrized)
   -- for a particular call in the target program.
   --
   -- This abstract type is intended as the root of processor-specific
   -- concrete types to represent the calling protocols used in the
   -- target processor and the target program.
   --
   -- The protocol may depend on some dynamic values, such as stack heights
   -- or frame pointers, and this is why it is defined as "boundable".
   -- However, a fully static protocol can also be represented and is
   -- indicated by a Basis function that returns a null list of cells.


   function Static (Item : Protocol_T) return Boolean
   is abstract;
   --
   -- Whether the protocol is fully bounded (static), either because
   -- it was created as a static protocol or because it has been
   -- bounded to become static.
   --
   -- The result of this function shall be equivalent to the
   -- condition Basis (Item)'Length = 0.


   function Map_Kind (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Map_Kind_T
   is abstract;
   --
   -- Indicates whether and how the data location described by the
   -- given Callee cell (interpreted in the callee's frame) is accessed
   -- in the caller's frame, when input values are transferred from
   -- caller to callee on call, and vice versa on return, Under the
   -- given calling protocol.
   --
   -- Should classify local stack height cells as Privy cells.


   function Invariant (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Boolean;
   --
   -- Whether the given Callee cell is invariant across a call Under
   -- this calling protocol.
   --
   -- This means that although the callee may assign to the cell,
   -- from the caller's point of view the associated caller cell
   -- (if there is one) is not changed by the call, perhaps
   -- because the callee saves and restores it.
   --
   -- The default implementation considers a Callee cell invariant
   -- if Map_Kind (Callee) = Privy.


   function Caller_Cell (
      Callee : Storage.Cell_T;
      Under  : Protocol_T)
   return Storage.Cell_T
   is abstract;
   --
   -- The caller-frame cell that is associated with (describes the
   -- same data-location as) the given callee-frame cell, Under the
   -- given calling protocol.
   --
   -- If Callee is a dynamically mapped cell, the protocol must be
   -- sufficiently resolved to map the cell, otherwise No_Cell is
   -- returned.


   function Sure_Invariant (
      Caller : Storage.Cell_T;
      Under  : Protocol_T;
      Stub   : Stub_Level_T)
   return Boolean
   is abstract;
   --
   -- Whether the given Caller cell is surely invariant across a call
   -- Under this calling protocol, to a callee that has the given
   -- Stub level (not Calls_No_Stub), whatever actual subprograms
   -- are hidden by the stubs.
   --
   -- In unclear cases (for example, unresolved dynamic protocol)
   -- the safe choice is to return False.
   --
   -- A Caller cell that models the local height of a stack should be
   -- marked as a sure invariant only when the protocol ensures that
   -- the stack height is unchanged by any call, that is, no call can
   -- cause a net change in the stack height; every call pops as much
   -- as it pushes.


   type Protocol_Ref is access all Protocol_T'Class;
   --
   -- A reference to some kind of calling protocol.


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Protocol_T;
      Giving :    out Protocol_Ref)
   is abstract;
   --
   -- Applies the Bounds Upon the boundable protocol, perhaps Giving
   -- a more constrained / better defined protocol. The procedure
   -- shall return Giving as null if it cannot constrain the given
   -- protocol (Upon) by means of the Bounds.
   --
   -- Note that this operation does not override Storage.Bounds.Apply
   -- because it has a different parameter profile.


   --
   ---   Static calling protocols
   --


   type Static_Protocol_T is abstract new Protocol_T with null record;
   --
   -- A static calling protocol, that is a protocol that has no dynamically
   -- mapped cells.


   function Static (Item : Static_Protocol_T) return Boolean;
   --
   -- Returns True.
   --
   -- Overrides (implements) Calling.Static.


   function Basis (Item : Static_Protocol_T) return Storage.Cell_List_T;
   --
   -- Returns the empty list, since the protocol is static.
   -- Overrides (implements) Storage.Bounds.Basis.


   procedure Apply (
      Bounds : in     Storage.Bounds.Bounds_T'Class;
      Upon   : in     Static_Protocol_T;
      Giving :    out Protocol_Ref);
   --
   -- Returns Giving as Null, because a static protocol cannot be
   -- further constrained. In fact there is no reason to call this
   -- operation.
   --
   -- Overrides (implements) Calling.Apply.


   --
   ---   Return methods
   --


   type Return_Way_T is (
      No_Return,
      Normal_Return,
      Offset_Return);
   --
   -- Some ways in which a subprogram may (or may not) return to its caller.
   --
   -- No_Return
   --    The subprogram never returns to its caller.
   --    This literal must be first in the enumeration (see the
   --    subtype Return_Some_Way_T below).
   -- Normal_Return
   --    The subprogram returns to the return address offered by the
   --    caller, according to the normal call/return mechanism defined
   --    by the processor's instruction set, or by the compiler's
   --    software conventions.
   -- Offset_Return
   --    The subprogram returns to a point in the program that is
   --    defined by a constant offset added to (or perhaps subtracted
   --    from) the normal return address (as in Normal_Return) offered
   --    by the caller. This method is used by some subprograms that
   --    use constant data located in the code memory immediately after
   --    the call instruction. The subprogram returns to the point after
   --    the constant data.
   --
   -- The actual point of return of a given call depends both on the
   -- callee's Return_Way and on the return address offered by the
   -- caller. For example, even if the callee uses Normal_Return, the
   -- caller may offer its own return address, making the callee return
   -- to the caller's caller (optimized tail call).


   subtype Return_Some_Way_T is Return_Way_T
      range Return_Way_T'Succ (No_Return) .. Return_Way_T'Last;
   --
   -- All the ways to (really) return.


   type Return_Method_T (Way : Return_Way_T := Normal_Return) is record

      case Way is

      when No_Return | Normal_Return =>

         null;

      when Offset_Return =>

         Offset : Processor.Code_Offset_T;

      end case;

   end record;
   --
   -- Defines if and how a subprogram returns to its caller, which
   -- combines a return Way and the possible parameters to the Way.
   --
   -- Offset
   --    If the caller offers the return address R, the callee returns
   --    to the address R + Offset.


   function Image (Item : Return_Method_T) return String;
   --
   -- A readable description of the return method.


end Calling;
