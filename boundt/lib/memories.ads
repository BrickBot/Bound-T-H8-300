-- Memories (decl)
--
-- Memory images containing some loaded code and/or data.
--
-- An instance of this generic package defines a type that represents
-- the memory of the target processor (or a part of the memory) that
-- may contain code or data or both. This data structure is an interface
-- between the target program as given in an executable file, and the
-- program's memory image as accessed during execution (or, in our case,
-- decoding). The code and data in the executable file are "loaded"
-- into one or more instances of this types (defined by one or more
-- instances of this package), and these instances then deliver
-- instructions and constant data to the Decoder on request.
-- 
-- The package is generic in the memory address type and the memory
-- storage unit (datum, word) type.
--
-- Conceptually, the memory is a vector, indexed by the address
-- type, and with elements of the memory unit type. However, the
-- vector may be defined for a subset of the address range only,
-- for example only for some "segments". Thus, this package can
-- tell a user if a specific memory location is defined (loaded)
-- or undefined, and in the former case what value was loaded.
-- Executable files often supply more information about a memory
-- address/location, for example whether it is executable and
-- whether it is read-only. This package does not support such
-- attributes, since they are often target-specific.
-- 
-- For loading data or code into the memory, the structure supports
-- both bulk-loading of contiguous segments and scatter-loading of
-- shorter pieces (TBC).
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
-- $Revision: 1.4 $
-- $Date: 2015/10/24 20:53:55 $
--
-- $Log: memories.ads,v $
-- Revision 1.4  2015/10/24 20:53:55  niklas
-- Moved to free licence.
--
-- Revision 1.3  2008-11-08 17:46:30  niklas
-- Extended procedure Allocate to allow part or all of the address range
-- to be already allocated, and to allocate storage for those parts that
-- are not yet allocated. Removed exception Allocation_Overlap as no
-- longer necessary. Added function Allocated to help unit tests.
--
-- Revision 1.2  2007/01/25 21:25:38  niklas
-- BT-CH-0043.
--
-- Revision 1.1  2004/04/24 17:18:00  niklas
-- First version.
--


with Interval_Maps;


generic

   type Address_Type is mod <>;

   type Datum_Type is private;

   type Data_Type is array (Natural range <>) of Datum_Type;

   Deallocate : in out Boolean;
   --
   -- Option variable to enable or disable the use of
   -- Unchecked_Deallocation to discard unused heap memory.


package Memories is


   type Content_T is limited private;
   --
   -- The contents of a memory with the given Address_Type and
   -- Datum_Type.
   --
   -- The memory is initially empty (no contents). Data can then
   -- be loaded into some memory locations (addresses) while other
   -- locations remain empty (undefined values).


   subtype Offset_T is Natural;
   --
   -- The offset between two addresses and the length of a memory
   -- area are here handled as Natural numbers, with the hope that
   -- this predefined type has enough range for typical target
   -- processors.


   procedure Initialize (Item : in out Content_T);
   --
   -- Initializes (creates) an instance of the memory contents.
   -- Every instance of Content_T should be initialized in this
   -- way before it is used in any other operation.


   function Loaded (
      Address : Address_Type;
      Within  : Content_T)
   return Boolean;
   --
   -- Whether the memory unit with the given address has been
   -- defined (loaded).


   function Loaded (
      Address : Address_Type;
      Span    : Offset_T;
      Within  : Content_T)
   return Boolean;
   --
   -- Whether the Span + 1 memory units at the addresses
   -- Address .. Address + Span have (all) been defined
   -- (loaded).


   procedure Allocate (
      Address : in     Address_Type;
      Span    : in     Offset_T;
      Within  : in out Content_T);
   --
   -- Creates, in advance of actual loading, storage for a
   -- segment of memory starting at the given address and
   -- able to contain Span + 1 memory units, that is, storage
   -- for the address range Address .. Address + Span.
   --
   -- It is TBA never necessary to pre-allocate storage in this way,
   -- but it may significantly speed up the loading of large data
   -- strings, especially if the single-Datum Load operation
   -- is used. IN THE PRESENT VERSION (TBM) PRE-ALLOCATION IS ALWAYS
   -- REQUIRED BEFORE LOADING DATA.


   function Allocated (Address : Address_Type; Within : Content_T)
   return Boolean;
   --
   -- Whether the space is allocated Within this object for storing
   -- a datum at the given Address.


   Memory_Not_Allocated : exception;
   --
   -- Raised upon an attempt to Load data at an address that has
   -- not been pre-allocated. TBM, this should be allowed and
   -- allocation should be automatic.


   procedure Load (
      Into   : in     Address_Type;
      Datum  : in     Datum_Type;
      Within : in out Content_T);
   --
   -- Loads a datum into the memory at the given address.


   procedure Load (
      Into   : in     Address_Type;
      Data   : in     Data_Type;
      Within : in out Content_T);
   --
   -- Loads a string of data into the memory at the addresses
   -- Into + Data'First .. Into + Data'Last.


   Undefined_Value : exception;
   --
   -- Raised if a client accesses an undefined (unloaded)
   -- memory location.


   function Datum (From : Address_Type; Within : Content_T)
   return Datum_Type;
   --
   -- The datum loaded at the given memory address.
   -- Raises Undefined_Value if the value is not defined.


   procedure Fetch (
      From   : in     Address_Type;
      Within : in     Content_T;
      Into   :    out Data_Type);
   --
   -- Fetches the loaded data from the memory addresses
   -- From+Into'First .. From+Into'Last and returns them
   -- in Into.
   -- Raises Undefined_Value if some of these addresses are
   -- not loaded.


   procedure Show_Maps (Item : in Content_T);
   --
   -- Displays the memory segment maps and load maps on standard
   -- output.


private


   type Data_Ref is access Data_Type;
   --
   -- Refers to a dynamically allocated data string that holds
   -- memory data for some interval of addresses.


   type Segment_T is record
      Address : Address_Type;
      Data    : Data_Ref;
   end record;
   --
   -- One segment of the memory content, holding the data for the
   -- memory addresses Address .. Address + Data'Length - 1.
   -- The data for address A is in Data(A - Address).
   -- Note that some of the locations may (still) be undefined
   -- (not yet loaded). However, _if_ some of these addresses have
   -- been loaded, they are stored in _this_ Data array, and not
   -- somewhere else.


   function Image (Item : Segment_T) return String;
   --
   -- Only for Show.


   package Data_Maps is new
      Interval_Maps (
         Index_Type => Address_Type,
         Value_Type => Segment_T,
         Image      => Image,
         Deallocate => Deallocate);
   --
   -- For managing the data (segments) in memory contents.


   type Data_Map_T is new Data_Maps.Interval_Map_T;


   type Load_Status_T is (Loaded);
   --
   -- Whether a given memory location has been loaded.
   -- The "not loaded" state is indicated by an undefined value
   -- in the load-map.


   function Image (Item : Load_Status_T) return String;
   --
   -- Only for Show.


   package Load_Maps is new
      Interval_Maps (
         Index_Type => Address_Type,
         Value_Type => Load_Status_T,
         Image      => Image,
         Deallocate => Deallocate);
   --
   -- For keeping track of which memory locations have been
   -- loaded.


   type Load_Map_T is new Load_Maps.Interval_Map_T;


   type Content_T is limited record
      Data_Map : Data_Map_T;
      Load_Map : Load_Map_T;
   end record;
   --
   -- A set of memory contents.
   --
   -- Data_Map
   --    Holds the allocated memory segments.
   -- Load_Map
   --    Records which memory locations have been loaded.
   --    For loaded locations, Load_Map is defined.
   --    For unloaded locations, Load_Map is not defined.
   --
   -- The domain of Load_Map is a subset of the domain of
   -- Data_Map. In other words, if Load_Map is defined for an
   -- address, so is Data_Map.


end Memories;
