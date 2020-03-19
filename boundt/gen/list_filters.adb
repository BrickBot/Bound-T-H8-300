-- List_Filters (body)
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
-- $Date: 2015/10/24 20:05:49 $
--
-- $Log: list_filters.adb,v $
-- Revision 1.2  2015/10/24 20:05:49  niklas
-- Moved to free licence.
--
-- Revision 1.1  2000-07-14 20:34:13  holsti
-- First version.
--


package body List_Filters is


   function Chosen (From : List_Type; Pick : Value_Type) return List_Type
   is

      Picked : List_Type (1 .. From'Length);
      Last   : Natural := 0;
      -- Accumulate the results in Picked(1 .. Last).

   begin

      for I in From'range loop

         if Value (From(I)) = Pick then
            -- This item to be included.
            Last := Last + 1;
            Picked(Last) := From(I);
         end if;

      end loop;

      return Picked(1 .. Last);

   end Chosen;


end List_Filters;
