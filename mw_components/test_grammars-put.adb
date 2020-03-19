with Number_Images;

separate (Test_Grammars)

procedure Put (File        : in Text_IO.File_Type := Current_Output;
               The_Grammar : in Character_Grammars.Grammar) is

   Last_Left_Part        : Nonterminal_Symbol;
   Last_Left_Part_Valid  : Boolean := False;
   Alignment_Column      : Text_IO.Positive_Count;
   Last_Column           : constant := 80;


   function Image (N : Nonterminal_Symbol) return String is

      function Image is new Number_Images.Integer_Image(Natural);

   begin
      if N.Number = 0 then
         return (1 => N.Letter);
      else
         return N.Letter & Image(N.Number);
      end if;
   end Image;


   procedure Put (The_Production : in Character_Grammars.Production) is

      function Image (S : Character_Grammars.Symbol_String) return String is

         function Image (S : Symbol) return String is
         begin
            case S.Kind is
               when Terminal =>
                  return (1 => S.Terminal_Value);
               when Nonterminal =>
                  return Image(S.Nonterminal_Value);
            end case;
         end Image;

      begin
         if S'Length = 0 then
            return "";
         elsif S'Length = 1 then
            return Image(S(S'First));
         else
            return Image(S(S'First)) & ' ' & Image(S(S'First + 1..S'Last));
         end if;
      end Image;

   begin
      if Last_Left_Part_Valid and then The_Production.Left = Last_Left_Part then
         Put(File, " |");
         if Col(File) + Image(To_String(The_Production.Right))'Length + 2 > Last_Column then
            New_Line(File);
            Set_Col(File, To => Alignment_Column);
         else
            Put(File, ' ');
         end if;
      else
         New_Line(File);
         Put(File, Image(The_Production.Left) & " -> ");
         Alignment_Column := Col(File);
         Last_Left_Part := The_Production.Left;
         Last_Left_Part_Valid := True;
      end if;
      Put(File, Image(To_String(The_Production.Right)));
   end Put;

   procedure Put_All is new Character_Grammars.Production_Bags.Traversal(Action => Put);

begin
   Put_Line(File, Image(The_Grammar.Start));
   Put_All(The_Grammar.Productions);
   New_Line(File);
end Put;
