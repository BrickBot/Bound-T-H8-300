-- Formats.In_Memory.Text (body)
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
-- $Date: 2015/10/24 20:53:53 $
--
-- $Log: formats-in_memory-text.adb,v $
-- Revision 1.2  2015/10/24 20:53:53  niklas
-- Moved to free licence.
--
-- Revision 1.1  2004-04-24 18:01:40  niklas
-- First version.
--


with Ada.Characters.Handling;
with Ada.Text_IO;
with Hex;


package body Formats.In_Memory.Text  is


   procedure Dump (
      Stream : in Stream_Ref)
   is
      use Ada.Text_IO;
      use type Index_T;

      Elements_Per_Line : constant := 10;
      -- Number of elements displayed per output line.

      Hex_Col : constant := 9;
      -- Column where first element is printed in hex.

      As_Char_Col :constant := Hex_Col + 3 * Elements_Per_Line + 5;
      -- Column where the first element is printed as character.

      Org_Index : constant Index_T := Index (Stream.all);
      -- The stream index position on entry; to be restored.

      Ind : Index_T;
      -- The index of the current element.

      Elm : Unsigned_8_T;
      -- The octet at Ind.

      Char_Elm : Character;
      -- The character with code Elm.

      As_Char : String (1 .. Elements_Per_Line);
      -- The elements of one output line, interpreted as characters.

      Last_Char : Natural := 0;
      -- The index of the last character in As_Char.
      -- If > 0, the characters are not yet printed.

      procedure Put_Characters
      --
      -- Prints out As_Char if it contains anything.
      --
      is
      begin

         if Last_Char > 0 then

            Set_Col (As_Char_Col);

            for A in 1 .. Last_Char loop

               Put (' ');
               Put (As_Char(A));

            end loop;

            Last_Char := 0;

         end if;

      end Put_Characters;


   begin  -- Dump

      Put_Line (
           "Formats.In_Memory.Stream, Length ="
         & Count_T'Image (Stream.Length));

      Set_Index (Stream => Stream.all, To => Index_T'First);

      loop

         exit when End_Of_Data (Stream.all);

         Ind := Index (Stream.all);

         if (Ind - Index_T'First) mod Elements_Per_Line = 0 then
            -- New line.

            Put_Characters;
            Set_Col (1);
            Put (Index_T'Image (Ind));
            Set_Col (9);

         end if;

         Unsigned_8_T'Read (Stream, Elm);

         Put (' '); Put (Hex.Image (Hex.Byte_T (Elm)));

         Last_Char := Last_Char + 1;

         Char_Elm := Character'Val (Elm);

         if Ada.Characters.Handling.Is_Graphic (Char_Elm) then

            As_Char(Last_Char) := Char_Elm;

         else

            As_Char(Last_Char) := '.';

         end if;

      end loop;

      Put_Characters;
      Set_Col(1);
      Put_Line ("end of stream data.");

      Set_Index (Stream => Stream.all, To => Org_Index);

   end Dump;


end Formats.In_Memory.Text;
