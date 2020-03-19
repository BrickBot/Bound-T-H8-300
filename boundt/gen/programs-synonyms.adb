-- Programs.Synonyms (body)
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
-- $Date: 2015/10/24 20:05:51 $
--
-- $Log: programs-synonyms.adb,v $
-- Revision 1.2  2015/10/24 20:05:51  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-07-27 20:24:19  niklas
-- BT-CH-0068. Option -synonyms.
--


with Output;
with Symbols;


package body Programs.Synonyms is


   Synonym_Key : constant String := "Synonym";
   --
   -- The key for the basic output lines.


   procedure Show (Subprogram : in Subprogram_T)
   is
      use Symbols;

      Sub_Name : constant String := Name (Subprogram, Qualified => True);
      -- The full name of this Subprogram.

      Conns : constant Connection_Set_T :=
         Connections_For_Address (
            Address => Entry_Address (Subprogram),
            Within  => Symbol_Table (Program (Subprogram)));
      -- All connections for this entry address.

      Mark : Output.Nest_Mark_T;
      -- For this Subprogram.

   begin

      Mark := Output.Nest (Locus (Subprogram, Qualified => True));

      for C in Conns'Range loop

         case Kind_Of (Conns(C)) is

         when Symbols.Subprogram
            | Symbols.Label       =>

            declare

               Syno_Name : constant String := Scoped_Name_Of (Conns(C));
               -- The synonym name.

            begin

               if Syno_Name /= Sub_Name then

                  Output.Result (
                     Key  => Synonym_Key,
                     Text => Scoped_Name_Of (Conns(C)));

               end if;

            end;

         when others =>

            null;

         end case;

      end loop;

      Output.Unnest (Mark);

   exception

   when others =>

      Output.Unnest (Mark);

      raise;

   end Show;


   procedure Show (Program : in Program_T)
   is

      Subs : constant Subprogram_List_T := Subprograms (Program);
      -- All subprograms so far defined in the Program.

   begin

      for S in Subs'Range loop

         Show (Subprogram => Subs(S));

      end loop;

   end Show;


end Programs.Synonyms;
