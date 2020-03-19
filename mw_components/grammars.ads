-- GENERIC PACKAGE FOR HANDLING GRAMMARS
   -------------------------------------

-- Creation : 22-FEB-1989 by Mats Weber.


with String_Operations,
     Generic_Varying_Text,
     Bags;

generic
   type Terminal_Symbol is private;
   type Nonterminal_Symbol is private;
   Null_Nonterminal : in Nonterminal_Symbol;
   Max_Right_Part_Length : in Natural;
   with function "<" (Left, Right : Terminal_Symbol)    return Boolean is <>;
   with function "<" (Left, Right : Nonterminal_Symbol) return Boolean is <>;
package Grammars is
----------------

   type Symbol_Kind is (Terminal, Nonterminal);

   type Symbol (Kind : Symbol_Kind := Nonterminal) is
   -----------
      record
         case Kind is
            when Terminal    => Terminal_Value    : Terminal_Symbol;
            when Nonterminal => Nonterminal_Value : Nonterminal_Symbol;
         end case;
      end record;

   function "<"  (Left, Right : Symbol) return Boolean;
   function ">"  (Left, Right : Symbol) return Boolean;
   function "<=" (Left, Right : Symbol) return Boolean;
   function ">=" (Left, Right : Symbol) return Boolean;


   type Symbol_String is array (Positive range <>) of Symbol;
   ------------------

   function "<"  (Left, Right : Symbol_String) return Boolean;
   function ">"  (Left, Right : Symbol_String) return Boolean;
   function "<=" (Left, Right : Symbol_String) return Boolean;
   function ">=" (Left, Right : Symbol_String) return Boolean;


   package Symbol_String_Handler is
   -----------------------------
      new String_Operations(Character  => Symbol,
                            String     => Symbol_String);

   use Symbol_String_Handler;


   package Symbol_Varying_Text is
   ---------------------------
      new Generic_Varying_Text(Character      => Symbol,
                               Null_Character => (Kind              => Nonterminal,
                                                  Nonterminal_Value => Null_Nonterminal),
                               String         => Symbol_String,
                               Side           => Symbol_String_Handler.Side);

   type Production is
   ---------------
      record
         Left  : Nonterminal_Symbol;
         Right : Symbol_Varying_Text.Text(Max_Length => Max_Right_Part_Length);
      end record;

   function Left_Part  (Of_Production : Production) return Nonterminal_Symbol;
   function Right_Part (Of_Production : Production) return Symbol_String;


   package Production_Bags is
   -----------------------
      new Bags(Key_Type  => Nonterminal_Symbol,
               Item_Type => Production,
               Key_Of    => Left_Part,
               Count     => Natural);

   package Access_By_Production is new Production_Bags.Access_By_Item;
   ----------------------------

   subtype Production_Bag is Production_Bags.Bag(Duplicate_Keys_Allowed => True);
   ----------------------


   type Grammar is
   ------------
      record
         Productions : Production_Bag;
         Start       : Nonterminal_Symbol := Null_Nonterminal;
      end record;


   generic
      with function New_Unused_Nonterminal
              (Associated_With : Nonterminal_Symbol;
               Not_In          : Production_Bag) return Nonterminal_Symbol;
         -- Must return a NONTERMINAL_SYMBOL that is not in NOT_IN.
   procedure Convert_To_Greibach_Normal_Form (The_Grammar : in out Grammar);
   -----------------------------------------

end Grammars;
