-- Formats.SDCC (decl)
--
-- Reading and analysing Small Device C Compiler (SDCC) file formats.
--
-- Author: Niklas Holsti, Tidorum Ltd, 2007.
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
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-sdcc.ads,v $
-- Revision 1.3  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.2  2007-09-23 20:25:37  niklas
-- Renamed Func_Or_Undefined to Undefined because functions are
-- always taken to be in the Code space.
--
-- Revision 1.1  2007/09/22 14:19:25  niklas
-- First version.
--


with Bags;           -- MW_Components
with String_Pool;


package Formats.SDCC is


   -- The references for this package are:
   --
   -- 1. "CDB File Format"
   --    Lenny Story, 2003-03-21, SDCC Development Team.


   --
   --    Types
   --


   type Type_Kind_T is (
      Void,
      Sbit,
      Char,
      Short,
      Int,
      Long,
      Flowt,
      Bit_Field,
      Aray,
      Generic_Pointer,
      Code_Pointer,
      Ext_RAM_Pointer,
      Int_RAM_Pointer,
      Paged_Pointer,
      Upper_128_Pointer,
      Func,
      Struct);
   --
   -- All the basic types and type constructors.
   --
   -- Apologies for "Flowt", but Float is predefined...


   subtype Primitive_Type_T is Type_Kind_T range Void .. Flowt;
   --
   -- These types are self-sufficient and can occur at the end
   -- of a type-chain (but are not the only such types).


   subtype Pointer_Type_T is
      Type_Kind_T range Generic_Pointer .. Upper_128_Pointer;
   --
   -- All kinds of pointer types.


   type Type_Layer_T (Kind : Type_Kind_T := Void)
   is record

      case Kind is

      when Primitive_Type_T
         | Pointer_Type_T => null;

      when Func =>

         Is_Interrupt : Boolean;
         Interrupt    : Natural;
         Reg_Bank     : Natural;

      when Bit_Field =>

         Start_Bit : Octet_Bit_T;
         Bits      : Natural;

      when Aray =>

         Length : Positive;

      when Struct =>

         Struct_Name : String_Pool.Item_T;

      end case;

   end record;
   --
   -- One layer in a type-chain.
   --
   -- Pointer_Type
   --    The "pointee" type is the next layer in the chain.
   -- Func
   --    The result type is the next layer in the chain.
   --    Is_Interrupt shows if the function is an interrupt handler.
   --    If so, Interrupt is the interrupt number and Reg_Bank is
   --    the number of the register bank that this function uses.
   -- Aray
   --    The element type is the next layer in the chain.
   -- Struct
   --    Can only occur as the last layer in a type chain.
   --    Use the Struct_Name to find the ""Type Record" that
   --    defines the contents (members) of the structure.


   type Type_Chain_T is array (Positive range <>) of Type_Layer_T;
   --
   -- A chain of types, listed in "outside-in" order. For example,
   -- if the first layer is a pointer type, the second layer is
   -- the type pointed to.


   No_Types : constant Type_Chain_T := (1 .. 0 => (Kind => Void));
   --
   -- A null type chain. A placeholder.


   type Type_Chain_Ref is access Type_Chain_T;
   --
   -- Refers to a chain of types on the heap.


   type Sign_T is (Signed, Unsigned);


   type Type_Def_T is record
      Size   : Natural;
      Chain  : Type_Chain_Ref;
      Sign   : Sign_T;
   end record;
   --
   -- A type definition.
   --
   -- Size
   --    The size, in octets, of a value of the "outermost" type,
   --    the type Chain(Chain'First).
   -- Chain
   --    The chain of pointer/array/function layers, from outermost
   --    to innermost.
   -- Sign
   --    The signedness of the "innermost" type, Chain(Chain'Last),
   --    if that is a type for which signedness is relevant.


   --
   --    Symbols
   --


   type Scope_Kind_T is (Global, File, Local);
   --
   -- The three kinds of scopes in C.


   type Scope_T (Kind : Scope_Kind_T := Global)
   is record

      case Kind is

      when Global =>

         null;

      when File => 

         File_Name : String_Pool.Item_T;

      when Local =>

         Func_Name : String_Pool.Item_T;
         Level     : Positive;
         Block     : Positive;

      end case;

   end record;
   --
   -- The scope of a symbol.


   function Image (Item : Scope_T) return String;
   --
   -- A description for human understanding.


   type Address_Space_T is (
      Ext_Stack,
      Int_Stack,
      Code,
      Code_Static,
      Int_RAM_Lower_128,
      Ext_RAM,
      Int_RAM,
      Bit,
      SFR,
      SBit,
      Register,
      Undefined);
   --
   -- The address spaces.
   --
   -- We always place functions in Code (or Code_Static TBC) even when
   -- the Space is marked as 'Z' = function or Undefined.


   subtype Stack_Space_T is Address_Space_T range Ext_Stack .. Int_Stack;
   --
   -- The spaces that are stacks of some sort.


   subtype Memory_Space_T is Address_Space_T range Code .. SBit;
   --
   -- The spaces that are (statically allocated) memory.


   type Reg_List_T is array (Positive range <>) of String_Pool.Item_T;
   --
   -- A list of register names.


   type Reg_List_Ref is access Reg_List_T;
   --
   -- Refers to a list of register names on the heap.


   function Image (Item : Reg_List_T) return String;
   --
   -- For human consumption.


   type Frame_Offset_T is new Integer;
   --
   -- The location of a stacked variable defined as an offset
   -- to the frame pointer ("bp").


   type Location_T (Space : Address_Space_T := Undefined)
   is record

      case Space is

      when Stack_Space_T =>

         Offset : Frame_Offset_T;

      when Memory_Space_T =>

         null;

      when Register =>

         Registers : Reg_List_Ref;

      when Undefined =>

         null;

      end case;

   end record;
   --
   -- The location of a symbol, as far as the compiler knows.
   -- Note that the address of memory-located symbols is not known
   -- because it is assigned by the linker.


   type Symbol_T is record
      Name     : String_Pool.Item_T;
      Scope    : Scope_T;
      Typ      : Type_Def_T;
      Location : Location_T;
   end record;
   --
   -- A symbol record, showing what the compiler knows about
   -- the symbol with this Name, in this Scope, to which the
   -- compiler has assigned this Location.
   --
   -- If the outermost Type layer is Func, the symbol represents
   -- a function (subprogram), and the remaining Type layers define
   -- the result type.
   --
   -- If the outermost Type layer is not Func, the symbol represents
   -- a data variable or constant of this Type.


   function Is_Function (Symbol : Symbol_T) return Boolean;
   --
   -- Whether this Symbol represents a function (subprogram).


   type Symbol_Table_T is limited private;
   --
   -- A table of Symbols, keyed by the Name and Scope.


   Duplicate_Symbol : exception;
   --
   -- Raised upon an attempt to insert a symbol into a table that
   -- already contains a symbol with the same Name and Scope.


   procedure Insert (
      Symbol : in     Symbol_T;
      Into   : in out Symbol_Table_T);
   --
   -- Inserts the Symbol Into the table. Propagates Duplicate_Symbol
   -- if the table already contains a symbol with the same Name
   -- and Scope.


   No_Such_Symbol : exception;
   --
   -- Raised upon a failed search for a symbol with a given Name
   -- and Scope in a given table.


   function Symbol (
      Name   : in String;
      Scope  : in Scope_T;
      Within : in Symbol_Table_T)
   return Symbol_T;
   --
   -- The symbol with the given Name and Scope, Within the given
   -- table. Propagates No_Such_Symbol is the table contains no
   -- such symbol.


private


   type Name_Scope_T is record
      Name  : String_Pool.Item_T;
      Scope : Scope_T;
   end record;
   --
   -- The symbol-table key.


   function Key_Of (Item : Symbol_T) return Name_Scope_T;
   --
   -- The key of a symbol.


   function "<" (Left, Right : Name_Scope_T) return Boolean;
   --
   -- Lexicographic comparison of Name and Scope, with Scope
   -- as the more significant component.


   -- The predefined "=" for Name_Scope_T is good enough because
   -- the predefined "=" for String_Pool.Item_T is equivalent to
   -- equality of the strings.


   package Symbol_Bags is new Bags (
      Key_Type   => Name_Scope_T,
      Item_Type  => Symbol_T,
      Key_Of     => Key_Of,
      Count      => Natural);

   type Symbol_Table_T is new Symbol_Bags.Bag (
      Duplicate_Keys_Allowed => False);


end Formats.SDCC;
