separate (Test_Grammars)

procedure Get (File        : in Text_IO.File_Type := Current_Input;
               The_Grammar : in out Character_Grammars.Grammar) is

   type Token_Kind is (Becomes, Bar, Semicolon, Dot, Symbol, Error);

   type Token (Kind : Token_Kind := Error) is
      record
         case Kind is
            when Symbol =>
               Symbol_Value : Character_Grammars.Symbol;
            when others =>
               null;
         end case;
      end record;

   Tok : Token;


   procedure Get (File      : in Text_IO.File_Type;
                  The_Token : out Token) is

      Ch : Character;

   begin
      loop
         Get(File, Ch);
         exit when Ch /= ' ' and Ch /= ASCII.HT;
      end loop;
      case Ch is
         when '-' =>
            Get(File, Ch);
            if Ch = '>' then
               The_Token := (Kind => Becomes);
            else
               The_Token := (Kind => Error);
            end if;
         when '|' =>
            The_Token := (Kind => Bar);
         when ';' =>
            The_Token := (Kind => Semicolon);
         when '.' =>
            The_Token := (Kind => Dot);
         when ''' =>
            Get(File, Ch);
            The_Token := (Kind         => Symbol,
                          Symbol_Value => (Kind           => Terminal,
                                           Terminal_Value => Ch));
            Get(File, Ch);
            if Ch /= ''' then
               The_Token := (Kind => Error);
            end if;
         when 'A'..'Z' =>
            The_Token := (Kind         => Symbol,
                          Symbol_Value => (Kind              => Nonterminal,
                                           Nonterminal_Value => (Letter => Ch,
                                                                 Number => 0)));
         when others =>
            The_Token := (Kind => Error);
      end case;
   exception
      when Text_IO.End_Error =>
         raise Syntax_Error;
   end Get;

begin
   Destroy(The_Grammar.Productions);
   Get(File, The_Token => Tok);
   begin
      The_Grammar.Start := Tok.Symbol_Value.Nonterminal_Value;
   exception
      when Constraint_Error =>
         raise Syntax_Error;
   end;
   Get(File, The_Token => Tok);
   if Tok.Kind /= Dot then
      raise Syntax_Error;
   end if;
   Production_Loop :
      loop
         declare

            New_Production : Character_Grammars.Production;

         begin
            Get(File, The_Token => Tok);
            begin
               New_Production.Left := Tok.Symbol_Value.Nonterminal_Value;
            exception
               when Constraint_Error =>
                  raise Syntax_Error;
            end;
            Get(File, The_Token => Tok);
            if Tok.Kind /= Becomes then
               raise Syntax_Error;
            end if;
            Bar_Loop :
               loop
                  Get(File, The_Token => Tok);
                  case Tok.Kind is
                     when Bar =>
                        Insert(Item => New_Production, Into => The_Grammar.Productions);
                        Truncate(New_Production.Right, Length => 0);
                     when Semicolon =>
                        Insert(Item => New_Production, Into => The_Grammar.Productions);
                        exit Bar_Loop;
                     when Dot =>
                        Insert(Item => New_Production, Into => The_Grammar.Productions);
                        exit Production_Loop;
                     when Symbol =>
                        Append(Tok.Symbol_Value, To => New_Production.Right);
                     when others =>
                        raise Syntax_Error;
                  end case;
               end loop Bar_Loop;
         end;
      end loop Production_Loop;
end Get;
