with Grammars,
     User_Interface,
     Text_IO;

use Text_IO;

procedure Test_Grammars is
-----------------------

   type Nonterminal_Symbol is
      record
         Letter : Character range 'A'..'Z';
         Number : Natural;
      end record;

   function "<" (Left, Right : Nonterminal_Symbol) return Boolean;


   Max_Right_Part_Length : constant := 100;

   package Character_Grammars is
      new Grammars(Terminal_Symbol       => Character,
                   Nonterminal_Symbol    => Nonterminal_Symbol,
                   Null_Nonterminal      => (Letter => 'Z',
                                             Number => Natural'Last),
                   Max_Right_Part_Length => Max_Right_Part_Length);

   use Character_Grammars,
       Character_Grammars.Symbol_Varying_Text,
       Character_Grammars.Production_Bags;

   function New_Unused_Nonterminal (Associated_With : Nonterminal_Symbol;
                                    Not_In          : Character_Grammars.Production_Bag) return Nonterminal_Symbol;

   procedure Greibachize is new Character_Grammars.Convert_To_Greibach_Normal_Form(New_Unused_Nonterminal);

   Syntax_Error : exception;  -- used in GET(GRAMMAR)


   function "<" (Left, Right : Nonterminal_Symbol) return Boolean is
   begin
      if Left.Letter /= Right.Letter then
         return Left.Letter < Right.Letter;
      else
         return Left.Number < Right.Number;
      end if;
   end "<";


   function New_Unused_Nonterminal (Associated_With : Nonterminal_Symbol;
                                    Not_In          : Character_Grammars.Production_Bag) return Nonterminal_Symbol is

      Result : Nonterminal_Symbol := Associated_With;

   begin
      loop
         Result.Number := Result.Number + 1;
         if not Member(Key => Result, Of_Bag => Not_In) then
            return Result;
         end if;
      end loop;
   end New_Unused_Nonterminal;


   procedure Get (File        : in Text_IO.File_Type := Current_Input;
                  The_Grammar : in out Character_Grammars.Grammar) is separate;

   procedure Put (File        : in Text_IO.File_Type := Current_Output;
                  The_Grammar : in Character_Grammars.Grammar) is separate;


begin
   declare

      A_Grammar    : Character_Grammars.Grammar;

      Input_File   : Text_IO.File_Type;

   begin
      Open(File => Input_File,
           Name => User_Interface.String_Answer("Initial Grammar File Name : "),
           Mode => In_File);
      Get(File => Input_File, The_Grammar => A_Grammar);
      Close(Input_File);
      Put_Line("Initial grammar :");
      Put(The_Grammar => A_Grammar);
      Greibachize(The_Grammar => A_Grammar);
      New_Line;
      Put_Line("Greibachized grammar :");
      Put(The_Grammar => A_Grammar);
   end;
end Test_Grammars;
