-- LEXICAL ANALYZER FOR THE ADA LANGUAGE
   -------------------------------------

-- Creation :  3-OCT-1988 by Mats Weber.


with Character_Handler,
     String_Case_Conversions,
     Number_Images;

use Character_Handler,
    String_Case_Conversions;

separate (Ada_Lexical_Analyzer)

package body Analyzer is
---------------------

   Current_Character         : Character;

   First_Character_Gotten    : Boolean;
   End_Of_Source_Gotten      : Boolean;

   Quote_Is_Delimiter        : Boolean;     -- True iff ''' must be a delimiter if it appears
                                            -- as the next non separator character.
   Was_Quote                 : Boolean;     -- True iff ''' as a delimiter
                                            -- was the last lexical element.

   Double_Dot_Follows,                      -- used when ".." directly follows an integer literal
   Becomes_Follows           : Boolean;     -- used when ":=" directly follows an integer literal


   procedure Reset is
   begin
      First_Character_Gotten := False;
      End_Of_Source_Gotten   := False;
      Quote_Is_Delimiter     := True;
      Was_Quote              := False;
      Double_Dot_Follows     := False;
      Becomes_Follows        := False;
   end Reset;


   procedure Get_Next_Lexical_Element (Element : out Lexical_Element) is

      Current_Element           : Lexical_Element;
      Current_Element_Image     : Lexical_Element_Image;

      Alternate_Character_Used  : Boolean := False;
                                  -- True iff an allowed replacement of characters
                                  -- is used in Current_Element.


      function Is_Imageable (The_Character : Character) return Boolean
         renames Character_Handler.Is_Graphic;


      procedure Get_Next_Character is
      begin
         Get(Current_Character);
      end;

      pragma Inline(Get_Next_Character);


   begin
      if not First_Character_Gotten then
         Get_Next_Character;
         First_Character_Gotten := True;
      end if;
      loop
         if Double_Dot_Follows then
            Current_Element := (Kind          => Delimiter,
                                Image         => To_Unbounded_String(".."),
                                The_Delimiter => Double_Dot);
            Double_Dot_Follows := False;
         elsif Becomes_Follows then
            Current_Element := (Kind          => Delimiter,
                                Image         => To_Unbounded_String(":="),
                                The_Delimiter => Becomes);
            Becomes_Follows := False;
         else
            -- Start a new lexical element
            Current_Element_Image := To_Unbounded_String(String'(1 => Current_Character));
            case Current_Character is
               when '&' =>
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => '&');
                  Get_Next_Character;
               when ''' =>
                  if Quote_Is_Delimiter then
                     Current_Element := (Kind          => Delimiter,
                                         Image         => Current_Element_Image,
                                         The_Delimiter => ''');
                  else
                     Get_Next_Character;
                     if Is_Imageable(Current_Character) then
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                     end if;
                     if Is_Graphic(Current_Character) then
                        Get_Next_Character;
                        if Is_Imageable(Current_Character) then
                           Append(New_Item => Current_Character, Source => Current_Element_Image);
                        end if;
                        if Current_Character = ''' then
                           Current_Element :=
                              (Kind                  => Character_Literal,
                               Image                 => Current_Element_Image,
                               The_Character_Literal =>
                                  Ada.Strings.Unbounded.Element(Current_Element_Image, Index => 2));
                        else
                           Current_Element := (Kind  => Lexical_Error,
                                               Image => Current_Element_Image);
                        end if;
                     else
                        Current_Element := (Kind  => Lexical_Error,
                                            Image => Current_Element_Image);
                     end if;
                  end if;
                  if Is_Imageable(Current_Character) then
                     Get_Next_Character;
                  end if;
               when '(' =>
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => '(');
                  Get_Next_Character;
               when ')' =>
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => ')');
                  Get_Next_Character;
               when '*' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '*' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Double_Star);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '*');
                  end case;
               when '+' =>
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => '+');
                  Get_Next_Character;
               when ',' =>
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => ',');
                  Get_Next_Character;
               when '-' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '-' =>
                        loop
                           Append(New_Item => Current_Character, Source => Current_Element_Image);
                           Get_Next_Character;
                           exit when not (Is_Graphic(Current_Character) or Current_Character = ASCII.HT);
                        end loop;
                        Current_Element := (Kind        => Comment,
                                            Image       => Current_Element_Image,
                                            The_Comment => Comment_Value(Current_Element_Image));
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '-');
                  end case;
               when '.' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '.' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Double_Dot);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '.');
                  end case;
               when '/' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '=' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Not_Equal);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '/');
                  end case;
               when ':' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '=' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Becomes);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => ':');
                  end case;
               when ';' =>
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => ';');
                  Get_Next_Character;
               when '<' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '<' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Left_Label_Bracket);
                        Get_Next_Character;
                     when '=' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Less_Than_Or_Equal);
                        Get_Next_Character;
                     when '>' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Box);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '<');
                  end case;
               when '=' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '>' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Arrow);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '=');
                  end case;
               when '>' =>
                  Get_Next_Character;
                  case Current_Character is
                     when '=' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Greater_Than_Or_Equal);
                        Get_Next_Character;
                     when '>' =>
                        Append(New_Item => Current_Character, Source => Current_Element_Image);
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => Right_Label_Bracket);
                        Get_Next_Character;
                     when others =>
                        Current_Element := (Kind          => Delimiter,
                                            Image         => Current_Element_Image,
                                            The_Delimiter => '>');
                  end case;
               when '|' | '!' =>
                  if Current_Character = '!' then
                     Alternate_Character_Used := True;
                  end if;
                  Current_Element := (Kind          => Delimiter,
                                      Image         => Current_Element_Image,
                                      The_Delimiter => '|');
                  Get_Next_Character;
               when 'A'..'Z' | 'a'..'z' =>
                  declare

                     Was_Underscore : Boolean := False;

                  begin
                     loop
                        Get_Next_Character;
                        case Current_Character is
                           when 'A'..'Z' | 'a'..'z' | '0'..'9' =>
                              Append(New_Item => Current_Character, Source => Current_Element_Image);
                              Was_Underscore := False;
                           when '_' =>
                              Append(New_Item => Current_Character, Source => Current_Element_Image);
                              if Was_Underscore then
                                 Current_Element := (Kind  => Lexical_Error,
                                                     Image => Current_Element_Image);
                                 Get_Next_Character;
                                 exit;
                              else
                                 Was_Underscore := True;
                              end if;
                           when others =>
                              if Was_Underscore then
                                 Current_Element := (Kind  => Lexical_Error,
                                                     Image => Current_Element_Image);
                              elsif not Was_Quote and
                                    Ada_Keywords.Is_Keyword(Word => To_String(Current_Element_Image))
                              then
                                 Current_Element :=
                                    (Kind              => Reserved_Word,
                                     Image             => Current_Element_Image,
                                     The_Reserved_Word => To_Unbounded_String
                                                             (Lower_Case
                                                                 (To_String
                                                                     (Current_Element_Image))));
                              else
                                 Current_Element :=
                                    (Kind           => Identifier,
                                     Image          => Current_Element_Image,
                                     The_Identifier => To_Unbounded_String
                                                          (Upper_Case
                                                             (To_String(Current_Element_Image))));
                              end if;
                              exit;
                        end case;
                     end loop;
                  end;
               when '0'..'9' =>
                  declare

                     Error_Found     : Boolean;
                     Is_Real_Literal : Boolean := False;

                     type Number_Base is range 2..16;


                     procedure Get_Simple_Numeric_Literal (Based         : in Boolean := False;
                                                           Base          : in Number_Base := 10;
                                                           Lexical_Error : out Boolean) is

                        Was_Underscore : Boolean := False;


                        function Digit_Allowed (The_Digit : Character;
                                                Base      : Number_Base) return Boolean is
                        begin
                           if Base <= 10 then
                              return The_Digit in '0'..Digit_Image(Digit_Number(Base - 1));
                           else
                              return The_Digit in '0'..'9' or
                                     Upper_Case(The_Digit) in 'A'..Digit_Image(Digit_Number(Base - 1));
                           end if;
                        end Digit_Allowed;

                     begin
                        if Is_Imageable(Current_Character) then
                           Append(New_Item => Current_Character, Source => Current_Element_Image);
                        end if;
                        if not Digit_Allowed(Current_Character, Base) then
                           Lexical_Error := True;
                           if Is_Imageable(Current_Character) then
                              Get_Next_Character;
                           end if;
                           return;
                        end if;
                        loop
                           Get_Next_Character;
                           case Current_Character is
                              when '0'..'9' | 'A'..'Z' | 'a'..'z' =>
                                 if not Based and then (Current_Character = 'E' or Current_Character = 'e') then
                                    Lexical_Error := Was_Underscore;
                                    exit;
                                 else
                                    Append(New_Item => Current_Character, Source => Current_Element_Image);
                                    Was_Underscore := False;
                                    if not Digit_Allowed(Current_Character, Base) then
                                       Lexical_Error := True;
                                       Get_Next_Character;
                                       exit;
                                    end if;
                                 end if;
                              when '_' =>
                                 Append(New_Item => Current_Character, Source => Current_Element_Image);
                                 if Was_Underscore then
                                    Lexical_Error := True;
                                    Get_Next_Character;
                                    exit;
                                 else
                                    Was_Underscore := True;
                                 end if;
                              when others =>
                                 Lexical_Error := Was_Underscore;
                                 exit;
                           end case;
                        end loop;
                     end Get_Simple_Numeric_Literal;

                  begin
                     Current_Element_Image := Null_Unbounded_String;
                     Get_Simple_Numeric_Literal(Lexical_Error => Error_Found);
                     if not Error_Found then
                        case Current_Character is
                           when '.' =>
                              Get_Next_Character;
                              if Current_Character = '.' then
                                 Double_Dot_Follows := True;
                                 Get_Next_Character;
                              else
                                 Is_Real_Literal := True;
                                 Append(New_Item => '.', Source => Current_Element_Image);
                                 Get_Simple_Numeric_Literal(Lexical_Error => Error_Found);
                              end if;
                           when '#' | ':' =>
                              declare

                                 Opener : constant Character := Current_Character;
                                 Base   : Number_Base;

                                 function Value is new Number_Images.Integer_Value(Number_Base);

                              begin
                                 Get_Next_Character;
                                 if Opener = ':' and Current_Character = '=' then
                                    Becomes_Follows := True;
                                    Get_Next_Character;
                                 else
                                    if Opener = ':' then
                                       Alternate_Character_Used := True;
                                    end if;
                                    begin
                                       Base := Value(To_String(Current_Element_Image));
                                    exception
                                       when Number_Images.Invalid_Number_String =>
                                          Error_Found := True;
                                    end;
                                    Append(New_Item => Opener, Source => Current_Element_Image);
                                    if not Error_Found then
                                       Get_Simple_Numeric_Literal(Based         => True,
                                                                  Base          => Base,
                                                                  Lexical_Error => Error_Found);
                                       if not Error_Found then
                                          if Current_Character = '.' then
                                             Is_Real_Literal := True;
                                             Append(New_Item => Current_Character, Source => Current_Element_Image);
                                             Get_Next_Character;
                                             Get_Simple_Numeric_Literal(Based         => True,
                                                                        Base          => Base,
                                                                        Lexical_Error => Error_Found);
                                             if not Error_Found then
                                                if Current_Character = Opener then
                                                   Append(New_Item => Current_Character, Source => Current_Element_Image);
                                                   Get_Next_Character;
                                                else
                                                   Error_Found := True;
                                                end if;
                                             end if;
                                          elsif Current_Character = Opener then
                                             Append(New_Item => Current_Character, Source => Current_Element_Image);
                                             Get_Next_Character;
                                          else
                                             Error_Found := True;
                                          end if;
                                       end if;
                                    end if;
                                 end if;
                              end;
                           when others =>
                              null;
                        end case;
                        if not Error_Found and not Double_Dot_Follows and not Becomes_Follows then
                           case Current_Character is
                              when 'E' | 'e' =>
                                 Append(New_Item => Current_Character, Source => Current_Element_Image);
                                 Get_Next_Character;
                                 if Current_Character = '+' or Current_Character = '-' then
                                    Append(New_Item => Current_Character, Source => Current_Element_Image);
                                    Get_Next_Character;
                                 end if;
                                 Get_Simple_Numeric_Literal(Lexical_Error => Error_Found);
                              when others =>
                                 null;
                           end case;
                        end if;
                     end if;
                     if Error_Found then
                        Current_Element := (Kind  => Lexical_Error,
                                            Image => Current_Element_Image);
                     elsif Is_Real_Literal then
                        declare

                           Real_Value : Real_Literal_Value;

                           function Value is new Number_Images.Float_Value(Real_Literal_Digits);

                        begin
                           begin
                              Real_Value := (Valid => True,
                                             Value => Value(To_String(Current_Element_Image)));
                           exception
                              when Number_Images.Invalid_Number_String =>
                                 Real_Value := (Valid => False);
                           end;
                           Current_Element := (Kind             => Real_Literal,
                                               Image            => Current_Element_Image,
                                               The_Real_Literal => Real_Value);
                        end;
                     else
                        declare

                           Integer_Value : Integer_Literal_Value;

                           function Value is new Number_Images.Integer_Value(Integer_Literal_Range);

                        begin
                           begin
                              Integer_Value := (Valid => True,
                                                Value => Value(To_String(Current_Element_Image)));
                           exception
                              when Number_Images.Invalid_Number_String =>
                                 Integer_Value := (Valid => False);
                           end;
                           Current_Element := (Kind                => Integer_Literal,
                                               Image               => Current_Element_Image,
                                               The_Integer_Literal => Integer_Value);
                        end;
                     end if;
                  end;
               when '"' | '%' =>
                  declare

                     Opener : constant Character := Current_Character;

                  begin
                     if Opener = '%' then
                        Alternate_Character_Used := True;
                     end if;
                     loop
                        Get_Next_Character;
                        if Is_Imageable(Current_Character) then
                           Append(New_Item => Current_Character, Source => Current_Element_Image);
                        end if;
                        if Current_Character = Opener then
                           Get_Next_Character;
                           if Current_Character = Opener then
                              Append(New_Item => Current_Character, Source => Current_Element_Image);
                           else
                              Current_Element := (Kind               => String_Literal,
                                                  Image              => Current_Element_Image,
                                                  The_String_Literal => Null_Unbounded_String);
                              for I in 2..Length(Current_Element_Image) - 1 loop
                                 if Ada.Strings.Unbounded.Element(Current_Element_Image,
                                                                  Index => I) /= Opener or else
                                    I mod 2 = 0
                                 then
                                    Append(New_Item => Ada.Strings.Unbounded.Element
                                                          (Current_Element_Image, Index => I),
                                           Source   => Current_Element.The_String_Literal);
                                 end if;
                              end loop;
                              exit;
                           end if;
                        elsif not Is_Graphic(Current_Character) then
                           Current_Element := (Kind  => Lexical_Error,
                                               Image => Current_Element_Image);
                           if Is_Imageable(Current_Character) then
                              Get_Next_Character;
                           end if;
                           exit;
                        end if;
                     end loop;
                  end;
               when ' ' | ASCII.HT | ASCII.VT | Line_Separator | Page_Separator =>
                  loop
                     Get_Next_Character;
                     case Current_Character is
                        when ' ' | ASCII.HT | ASCII.VT | Line_Separator | Page_Separator =>
                           Append(New_Item => Current_Character, Source => Current_Element_Image);
                        when others =>
                           exit;
                     end case;
                  end loop;
                  Current_Element := (Kind  => Separators,
                                      Image => Current_Element_Image);
               when File_Terminator =>
                  Current_Element := (Kind  => End_Of_Source,
                                      Image => Null_Unbounded_String);
               when others =>
                  Current_Element := (Kind  => Lexical_Error,
                                      Image => Current_Element_Image);
                  Get_Next_Character;
            end case;
         end if;
         if Current_Element.Kind /= Separators then
            Quote_Is_Delimiter :=
               Current_Element.Kind = Identifier or
               Current_Element.Kind = Character_Literal or
               Current_Element.Kind = String_Literal or
               (Current_Element.Kind = Delimiter and then Current_Element.The_Delimiter = ')') or
               (Current_Element.Kind = Reserved_Word and then Current_Element.The_Reserved_Word = "all") or
               Current_Element.Kind = Lexical_Error;
            Was_Quote := Current_Element.Kind = Delimiter and then Current_Element.The_Delimiter = ''';
         end if;
         if Alternate_Character_Used then
            if not Allow_Alternate_Characters then
               Current_Element := (Kind  => Lexical_Error,
                                   Image => Current_Element.Image);
            end if;
         end if;
         exit when (Include_Separators or Current_Element.Kind /= Separators) and
                   (Include_Comments or Current_Element.Kind /= Comment);
      end loop;
      if End_Of_Source_Gotten then
         raise Source_At_End;
      else
         End_Of_Source_Gotten := Current_Element.Kind = End_Of_Source;
         Element := Current_Element;
      end if;
   end Get_Next_Lexical_Element;


begin
   Reset;
end Analyzer;
