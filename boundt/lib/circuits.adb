-- Circuits (body)
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
-- $Log: circuits.adb,v $
-- Revision 1.2  2015/10/24 20:53:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2008-06-11 07:58:35  niklas
-- First version.
--


with Ada.Text_IO;


package body Circuits is


   use Ada.Text_IO;


   function Cycle (Circuit : Circuit_T) return Natural
   is
   begin

      return Circuit.Cycle;

   end Cycle;


   Part_Number : Natural := 0;
   --
   -- A sequential numbering of all parts.


   -- overriding
   procedure Initialize (Part : in out Root_Part_T)
   is

      Circuit : Circuit_T renames Part.Circuit.all;

   begin

      Part_Number := Part_Number + 1;

      Part.Number := Part_Number;

      Circuit.Num_Parts := Circuit.Num_Parts + 1;

      Circuit.Part(Circuit.Num_Parts) := Part'Unchecked_Access;

   end Initialize;


   procedure Run_Cycle (Circuit : in out Circuit_T)
   is
   begin

      for P in 1 .. Circuit.Num_Parts loop

         Compute (Circuit.Part(P).all);

      end loop;

      for P in 1 .. Circuit.Num_Parts loop

         Clock (Circuit.Part(P).all);

      end loop;

      Circuit.Cycle := Circuit.Cycle + 1;

   end Run_Cycle;


   --
   --    Parts that compute various types of values
   --


   package body Parts
   is

      -- overriding
      procedure Compute (Part : in out Part_T)
      is
      begin

         -- Put_Line ("Compute part #" & Positive'Image (Part.Number));

         Part.Computer (
            Circuit => Part.Circuit.all,
            Load    => Part.Load,
            Value   => Part.Last);

      end Compute;


      -- overriding
      procedure Clock (Part : in out Part_T)
      is
      begin

         -- Put_Line ("Clock part #" & Positive'Image (Part.Number));

         if Part.Load then
            -- Clock enabled.

            for L in reverse 1 .. Max_Latency loop

               Part.Pipeline(L) := Part.Pipeline(L - 1);

            end loop;

            Part.Pipeline(0) := Part.Last;

         end if;

      end Clock;


      function Output (
         From    : Part_T;
         Latency : Latency_T := 0)
      return Value_Type
      is
      begin

         return From.Pipeline(Latency);

      end Output;


      procedure Load (
         Value : in     Value_Type;
         Into  : in out Part_T)
      is
      begin

         Into.Last := Value;
         Into.Load := True;

         Clock (Part_T'Class (Into));

      end Load;


   end Parts;


end Circuits;
