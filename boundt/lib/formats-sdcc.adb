-- Formats.SDCC (body)
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
-- $Revision: 1.2 $
-- $Date: 2015/10/24 20:53:54 $
--
-- $Log: formats-sdcc.adb,v $
-- Revision 1.2  2015/10/24 20:53:54  niklas
-- Moved to free licence.
--
-- Revision 1.1  2007-09-22 14:19:24  niklas
-- First version.
--


package body Formats.SDCC is


   --
   --    Symbols
   --


   function Image (Item : Scope_T) return String
   is
   begin

      case Item.Kind is

      when Global =>

         return "global";

      when File => 

         return "file " & String_Pool.To_String (Item.File_Name);

      when Local =>

         return "func "
              & String_Pool.To_String (Item.Func_Name)
              & ", level"
              & Positive'Image (Item.Level)
              & ", block"
              & Positive'Image (Item.Block);

      end case;

   end Image;


   function Image (Item : Reg_List_T) return String
   is
   begin

      if Item'Length = 0 then

         return "";

      elsif Item'Length = 1 then

        return String_Pool.To_String (Item(Item'First));

      else

         return
              String_Pool.To_String (Item(Item'First))
            & ','
            & Image (Item(Item'First + 1 .. Item'Last));

      end if;

   end Image;


   function Is_Function (Symbol : Symbol_T) return Boolean
   is

      Chain : Type_Chain_T renames Symbol.Typ.Chain.all;

   begin

      return Chain(Chain'First).Kind = Func;

   end Is_Function;


   procedure Insert (
      Symbol : in     Symbol_T;
      Into   : in out Symbol_Table_T)
   is
   begin

      Symbol_Bags.Insert (
         Item => Symbol,
         Into => Symbol_Bags.Bag (Into));

   exception

   when Symbol_Bags.Duplicate_Key =>

      raise Duplicate_Symbol;

   end Insert;


   function Symbol (
      Name   : in String;
      Scope  : in Scope_T;
      Within : in Symbol_Table_T)
   return Symbol_T
   is
   begin

      return Symbol_Bags.Search (
         Key    => (String_Pool.To_Item (Name), Scope),
         Within => Symbol_Bags.Bag (Within));

   exception

   when Symbol_Bags.Nonexistent_Key =>

      raise No_Such_Symbol;

   end Symbol;


   function Key_Of (Item : Symbol_T) return Name_Scope_T
   is
   begin

      return (Item.Name, Item.Scope);

   end Key_Of;


   function "<" (Left, Right : Scope_T) return Boolean
   --
   -- For use in the comparison of Name_Scopes.
   --
   is
      use type String_Pool.Item_T;
   begin

      if Left.Kind = Right.Kind then

         case Left.Kind is

         when Global => return False;

         when File   => return Left.File_Name < Right.File_Name;

         when Local  =>

            return Left.Func_Name < Right.Func_Name
            or else (Left.Func_Name = Right.Func_Name
               and then (Left.Level < Right.Level
                  or else (Left.Level = Right.Level
                     and then Left.Block < Right.Block)));

         end case;

      else

         return Left.Kind < Right.Kind;

      end if;

   end "<";


   function "<" (Left, Right : Name_Scope_T) return Boolean
   is
      use type String_Pool.Item_T;
   begin

      return Left.Scope < Right.Scope
      or else (Left.Scope = Right.Scope
         and then Left.Name < Right.Name);

   end "<";


end Formats.SDCC;
