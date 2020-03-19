-- Flow.Checks (body)
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
-- $Date: 2015/10/24 20:05:47 $
--
-- $Log: flow-checks.adb,v $
-- Revision 1.2  2015/10/24 20:05:47  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-08-25 18:51:13  niklas
-- First version.
--


with Output;


package body Flow.Checks is


   procedure Fault (Text : in String)
   --
   -- Emits a fault message.
   --
   is
   begin

      Output.Fault (
         Location => "Flow.Checks",
         Text     => Text);

   end Fault;


   procedure After_Inserting_Step (
      Step : in Step_T;
      Into : in Graph_T)
   is

      Other : Step_T;
      -- Some other step. 

   begin

      -- Check that no other step has the same tag:

      for I in 1 .. Max_Step (Into) - 1 loop

         Other := Step_At (Index => I, Within => Into);

         if Tag (Other) = Tag (Step) then

            Fault (
                 Image (Other)
               & " has the same tag as inserted "
               & Image (Step));

         end if;

      end loop;

   end After_Inserting_Step;


end Flow.Checks;
