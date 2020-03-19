-- PROCEDURE TO CONVERT A GRAMMAR TO GREIBACH NORMAL FORM
   ------------------------------------------------------

-- Creation : 24-FEB-1989 by Mats Weber.


separate (Grammars)

procedure Convert_To_Greibach_Normal_Form (The_Grammar : in out Grammar) is
-----------------------------------------

   use Symbol_Varying_Text,
       Production_Bags,
       Access_By_Production;


   B : Nonterminal_Symbol;


   function Is_Directly_Left_Recursive (B : Nonterminal_Symbol) return Boolean is

      B_Is_Directly_Left_Recursive : exception;

      procedure Check (U : in Production) is
      begin
         if U.Left = B and (Length(U.Right) > 0 and then
                            Char(U.Right, Position => 1).Kind = Nonterminal and then
                            Char(U.Right, Position => 1).Nonterminal_Value = B)
         then
            raise B_Is_Directly_Left_Recursive;
         end if;
      end Check;

      procedure Check_All is new Production_Bags.Traversal(Action => Check);

   begin
      Check_All(The_Grammar.Productions);
      return False;
   exception
      when B_Is_Directly_Left_Recursive =>
         return True;
   end Is_Directly_Left_Recursive;


   procedure Eliminate_Direct_Left_Recursivity (B : in Nonterminal_Symbol) is

      B1                     : constant Nonterminal_Symbol :=
                               New_Unused_Nonterminal(Associated_With => B,
                                                      Not_In          => The_Grammar.Productions);

      Productions_To_Add,
      Productions_To_Remove  : Production_Bag;

      procedure Modify (U : in Production) is
      begin
         if U.Left = B then
            if Length(U.Right) > 0 and then
               Char(U.Right, Position => 1).Kind = Nonterminal and then
               Char(U.Right, Position => 1).Nonterminal_Value = B
            then
               Insert(Item => (Left  => B1,
                               Right => To_Text(Substring(U.Right, First => 2, Last => Length(U.Right)),
                                                Max_Length => Max_Right_Part_Length)),
                      Into => Productions_To_Add);
               Insert(Item => (Left  => B1,
                               Right => To_Text(Substring(U.Right, First => 2, Last  => Length(U.Right)) &
                                                Symbol'(Kind => Nonterminal, Nonterminal_Value => B1),
                                                Max_Length => Max_Right_Part_Length)),
                      Into => Productions_To_Add);
               Insert(Item => U,
                      Into => Productions_To_Remove);
            else
               Insert(Item => (Left  => B,
                               Right => To_Text(To_String(U.Right) &
                                                Symbol'(Kind => Nonterminal, Nonterminal_Value => B1),
                                                Max_Length => Max_Right_Part_Length)),
                      Into => Productions_To_Add);
            end if;
         end if;
      end Modify;

      procedure Modify_All is new Production_Bags.Traversal(Action => Modify);


      procedure Remove_From_The_Grammar (P : in Production) is
      begin
         Remove(Item => P, From => The_Grammar.Productions);
      end;

      procedure Remove_All_From_The_Grammar is new Production_Bags.Traversal(Action => Remove_From_The_Grammar);

   begin
      Modify_All(The_Grammar.Productions);
      Remove_All_From_The_Grammar(Productions_To_Remove);
      Destroy(Productions_To_Remove);
      Insert(Items => Productions_To_Add, Into => The_Grammar.Productions);
      Destroy(Productions_To_Add);
   end Eliminate_Direct_Left_Recursivity;


   procedure Eliminate_Indirect_Recursivity (U : in Production) is

      Productions_To_Add : Production_Bag;

      procedure Modify (W : in Production) is
      begin
         if Length(U.Right) > 0 and then
            Char(U.Right, Position => 1).Kind = Nonterminal and then
            W.Left = Char(U.Right, Position => 1).Nonterminal_Value
         then
            Insert(Item => (Left  => U.Left,
                            Right => To_Text(To_String(W.Right) &
                                             Substring(U.Right, First => 2, Last => Length(U.Right)),
                                             Max_Length => Max_Right_Part_Length)),
                   Into => Productions_To_Add);
         end if;
      end Modify;

      procedure Modify_All is new Production_Bags.Traversal(Action => Modify);

   begin
      Modify_All(The_Grammar.Productions);
      Remove(Item => U, From => The_Grammar.Productions);
      Insert(Items => Productions_To_Add, Into => The_Grammar.Productions);
      Destroy(Productions_To_Add);
   end Eliminate_Indirect_Recursivity;


begin
   loop
      declare

         Not_Finished : exception;

         procedure Check (U : in Production) is
         begin
            if Length(U.Right) > 0 and then
               Char(U.Right, Position => 1).Kind = Nonterminal
            then
               B := Char(U.Right, Position => 1).Nonterminal_Value;
               raise Not_Finished;
            end if;
         end Check;

         procedure Check_All is new Production_Bags.Traversal(Action => Check);

      begin
         Check_All(The_Grammar.Productions);
         exit;
      exception
         when Not_Finished =>
            null;
      end;
      if Is_Directly_Left_Recursive(B) then
         Eliminate_Direct_Left_Recursivity(B);
      else
         declare

            No_More_Productions_To_Process : exception;

            function Next_Production_To_Process return Production is

               Result        : Production;

               Result_Found  : exception;

               procedure Check (U : in Production) is
               begin
                  if Length(U.Right) > 0 and then
                     Char(U.Right, Position => 1).Kind = Nonterminal and then
                     Char(U.Right, Position => 1).Nonterminal_Value = B
                  then
                     Result := U;
                     raise Result_Found;
                  end if;
               end Check;

               procedure Check_All is new Production_Bags.Traversal(Action => Check);

            begin
               Check_All(The_Grammar.Productions);
               raise No_More_Productions_To_Process;
            exception
               when Result_Found =>
                  return Result;
            end Next_Production_To_Process;

         begin
            loop
               begin
                  Eliminate_Indirect_Recursivity(Next_Production_To_Process);
               exception
                  when No_More_Productions_To_Process =>
                     exit;
               end;
            end loop;
         end;
      end if;
   end loop;
end Convert_To_Greibach_Normal_Form;
