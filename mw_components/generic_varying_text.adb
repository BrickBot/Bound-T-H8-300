-- GENERIC TEXT HANDLING PACKAGE
   -----------------------------

-- Creation :  5-JUL-1986 by Mats Weber.


package body Generic_Varying_Text is
---------------------------------

   function Length (Of_Text : Text) return Index is
   begin
      return Of_Text.Last;
   end;


   function Empty (The_Text : Text) return Boolean is
   begin
      return The_Text.Last = 0;
   end;


   function To_String (The_Text : Text) return String is
   begin
      return The_Text.Value(1..The_Text.Last);
   end;


   function Char (Of_Text : Text; Position : Index) return Character is
   begin
      if Position not in 1..Of_Text.Last then
         raise Index_Error;
      end if;
      return Of_Text.Value(Position);
   end;


   function To_Text (Value : Character) return Text is
   begin
      return Text'(Max_Length | Last => 1, Value => (1 => Value));
   end;

   function To_Text (Value : String) return Text is

      subtype String_1_Value is String(1..Value'Length);

   begin
      return Text'(Max_Length |
                   Last       => Value'Length,
                   Value      => String_1_Value(Value));
   end To_Text;

   function To_Text (Value : Character; Max_Length : Index) return Text is

      Result : Text(Max_Length);

   begin
      if Max_Length < 1 then
         raise Length_Error;
      end if;
      Result.Last := 1;
      Result.Value(1) := Value;
      return Result;
   end To_Text;

   function To_Text (Value : String; Max_Length : Index) return Text is

      Result : Text(Max_Length);

   begin
      if Max_Length < Value'Length then
         raise Length_Error;
      end if;
      Result.Last := Value'Length;
      Result.Value(1..Value'Length) := Value;
      return Result;
   end To_Text;


   procedure Assign (Object : out Text; Value : in Character) is
   begin
      if Object.Max_Length < 1 then
         raise Length_Error;
      end if;
      Object.Last := 1;
      Object.Value(1) := Value;
      Object.Value(2..Object.Max_Length) := (others => Null_Character);
   end Assign;

   procedure Assign (Object : out Text; Value : in String) is
   begin
      if Object.Max_Length < Value'Length then
         raise Length_Error;
      end if;
      Object.Last := Value'Length;
      Object.Value(1..Value'Length) := Value;
      Object.Value(Value'Length + 1..Object.Max_Length) := (others => Null_Character);
   end Assign;

   procedure Assign (Object : out Text; Value : in Text) is
   begin
      if Object.Max_Length < Value.Last then
         raise Length_Error;
      end if;
      Object.Last := Value.Last;
      Object.Value(1..Value.Last) := Value.Value(1..Value.Last);
      Object.Value(Value.Last + 1..Object.Max_Length) := (others => Null_Character);
   end Assign;


   procedure Assign (Object : in out Text; Position : in Index; Value : in Character) is
   begin
      if Position not in 1..Object.Last + 1 then
         raise Index_Error;
      elsif Position > Object.Max_Length then
         raise Length_Error;
      end if;
      Object.Value(Position) := Value;
      if Position > Object.Last then
         Object.Last := Position;
      end if;
   end Assign;

   procedure Assign (Object : in out Text; Position : in Index; Value : in String) is

      Last_Position : constant Index := Position + Value'Length - 1;

   begin
      if Position not in 1..Object.Last + 1 then
         raise Index_Error;
      elsif Last_Position > Object.Max_Length then
         raise Length_Error;
      end if;
      Object.Value(Position..Last_Position) := Value;
      if Last_Position > Object.Last then
         Object.Last := Last_Position;
      end if;
   end Assign;

   procedure Assign (Object : in out Text; Position : in Index; Value : in Text) is
   begin
      Assign(Object, Position, Value.Value(1..Value.Last));
   end Assign;


   function "&" (T : Text; U : Text) return Text is
   begin
      return Text'(Max_Length |
                   Last       => T.Last + U.Last,
                   Value      => T.Value(1..T.Last) & U.Value(1..U.Last));
   end "&";

   function "&" (T : Text; U : String) return Text is

      subtype String_1_U is String(1..U'Length);

   begin
      return Text'(Max_Length |
                   Last       => T.Last + U'Length,
                   Value      => T.Value(1..T.Last) & String_1_U(U));
   end "&";

   function "&" (T : String; U : Text) return Text is

      subtype String_1_T is String(1..T'Length);

   begin
      return Text'(Max_Length |
                   Last       => T'Length + U.Last,
                   Value      => String_1_T(T) & U.Value(1..U.Last));
   end "&";

   function "&" (T : Text; U : Character) return Text is
   begin
      return Text'(Max_Length |
                   Last       => T.Last + 1,
                   Value      => T.Value(1..T.Last) & U);
   end "&";

   function "&" (T : Character; U : Text) return Text is
   begin
      return Text'(Max_Length |
                   Last       => 1 + U.Last,
                   Value      => T & U.Value(1..U.Last));
   end "&";


   function Equal (T : Text; U : Text) return Boolean is
   begin
      return T.Value(1..T.Last) = U.Value(1..U.Last);
   end Equal;

   function Equal (T : Text; U : String) return Boolean is
   begin
      return T.Value(1..T.Last) = U;
   end Equal;

   function Equal (T : String; U : Text) return Boolean is
   begin
      return T = U.Value(1..U.Last);
   end Equal;


   function "<" (T,U : Text) return Boolean is
   begin
      return T.Value(1..T.Last) < U.Value(1..U.Last);
   end "<";

   function ">" (T,U : Text) return Boolean is
   begin
      return T.Value(1..T.Last) > U.Value(1..U.Last);
   end ">";

   function "<=" (T,U : Text) return Boolean is
   begin
      return T.Value(1..T.Last) <= U.Value(1..U.Last);
   end "<=";

   function ">=" (T,U : Text) return Boolean is
   begin
      return T.Value(1..T.Last) >= U.Value(1..U.Last);
   end ">=";


   function Scan (Within : Text;
                  From   : Side := Left) return Index is
   begin
      if From = Left then
         for I in 1..Within.Last loop
            if Condition(Within.Value(I)) then
               return I;
            end if;
         end loop;
         return Within.Last + 1;
      elsif From = Right then
         for I in reverse 1..Within.Last loop
            if Condition(Within.Value(I)) then
               return I;
            end if;
         end loop;
         return 0;
      end if;
   end Scan;


   procedure Transformation (On_Text : in out Text) is
   begin
      for I in 1..On_Text.Last loop
         Transform(On_Text.Value(I));
      end loop;
   end Transformation;


   function Locate (Pattern : Character;
                    Within  : Text;
                    From    : Side := Left) return Index is
   begin
      return Locate(Pattern, Within.Value(1..Within.Last), From);
   end Locate;

   function Locate (Pattern : String;
                    Within  : Text;
                    From    : Side := Left) return Index is
   begin
      return Locate(Pattern, Within.Value(1..Within.Last), From);
   end Locate;

   function Locate (Pattern : Text;
                    Within  : Text;
                    From    : Side := Left) return Index is
   begin
      return Locate(Pattern.Value(1..Pattern.Last),
                    Within.Value(1..Within.Last),
                    From);
   end Locate;


   function Located (Pattern : Character; Within : Text) return Boolean is
   begin
      return Located(Pattern, Within.Value(1..Within.Last));
   end Located;


   function Located (Pattern : String; Within : Text) return Boolean is
   begin
      return Located(Pattern, Within.Value(1..Within.Last));
   end Located;


   function Located (Pattern : Text; Within : Text) return Boolean is
   begin
      return Located(Pattern.Value(1..Pattern.Last),
                     Within.Value(1..Within.Last));
   end Located;


   function Substring (Of_Text : Text; First, Last : Index) return Text is
   begin
      if First <= Last then
         if First not in 1..Of_Text.Last or Last not in 1..Of_Text.Last then
            raise Index_Error;
         end if;
         declare

            subtype String_1_Result_Length is String(1..Last - First + 1);

         begin
            return Text'(Max_Length |
                         Last       => String_1_Result_Length'Last,
                         Value      => String_1_Result_Length(Of_Text.Value(First..Last)));
         end;
      else
         return Null_Text;
      end if;
   end Substring;

   function Substring (Of_Text : Text; First, Last : Index) return String is
   begin
      if First <= Last and then
         (First not in 1..Of_Text.Last or Last not in 1..Of_Text.Last)
      then
         raise Index_Error;
      end if;
      return Of_Text.Value(First..Last);
   end Substring;


   procedure Insert (Fragment : in Character;
                     Into     : in out Text;
                     Position : in Index) is
   begin
      if Position not in 1..Into.Last + 1 then
         raise Index_Error;
      end if;
      if Into.Last + 1 > Into.Max_Length then
         raise Length_Error;
      end if;
      Into.Value(Position + 1..Into.Last + 1) := Into.Value(Position..Into.Last);
      Into.Value(Position) := Fragment;
      Into.Last := Into.Last + 1;
   end Insert;

   procedure Insert (Fragment : in String;
                     Into     : in out Text;
                     Position : in Index) is
   begin
      if Position not in 1..Into.Last + 1 then
         raise Index_Error;
      end if;
      if Into.Last + Fragment'Length > Into.Max_Length then
         raise Length_Error;
      end if;
      Into.Value(Position + Fragment'Length..Into.Last + Fragment'Length) :=
         Into.Value(Position..Into.Last);
      Into.Value(Position..Position + Fragment'Length - 1) := Fragment;
      Into.Last := Into.Last + Fragment'Length;
   end Insert;

   procedure Insert (Fragment : in Text;
                     Into     : in out Text;
                     Position : in Index) is
   begin
      Insert(Fragment.Value(1..Fragment.Last), Into, Position);
   end Insert;


   procedure Append (Fragment : in Character; To : in out Text) is
   begin
      if To.Last >= To.Max_Length then
         raise Length_Error;
      end if;
      To.Last := To.Last + 1;
      To.Value(To.Last) := Fragment;
   end Append;

   procedure Append (Fragment : in String; To : in out Text) is
   begin
      if To.Last + Fragment'Length > To.Max_Length then
         raise Length_Error;
      end if;
      To.Value(To.Last + 1..To.Last + Fragment'Length) := Fragment;
      To.Last := To.Last + Fragment'Length;
   end Append;

   procedure Append (Fragment : in Text; To : in out Text) is
   begin
      Append(Fragment.Value(1..Fragment.Last), To);
   end Append;


   procedure Truncate (The_Text : in out Text; Length : in Index) is
   begin
      if Length not in 0..The_Text.Last then
         raise Index_Error;
      end if;
      The_Text.Value(Length + 1..The_Text.Last) := (others => Null_Character);
      The_Text.Last := Length;
   end Truncate;


   procedure Delete (Within : in out Text; First, Length : in Index) is
   begin
      if Length = 0 then
         if First in 1..Within.Last + 1 then
            return;
         else
            raise Index_Error;
         end if;
      end if;
      if First not in 1..Within.Last or First + Length - 1 not in 1..Within.Last then
         raise Index_Error;
      end if;
      Within.Value(First..Within.Last - Length) := Within.Value(First + Length..Within.Last);
      Within.Value(Within.Last - Length + 1..Within.Last) := (others => Null_Character);
      Within.Last := Within.Last - Length;
   end Delete;


   procedure Delete (Pattern : in Character;
                     Within  : in out Text;
                     From    : in Side := Left) is

      Result : constant String := Delete(Pattern,
                                         Within.Value(1..Within.Last),
                                         From);

   begin
      Within.Value(Result'Length + 1..Within.Last) := (others => Null_Character);
      Within.Last := Result'Length;
      Within.Value(1..Result'Length) := Result;
   end Delete;

   procedure Delete (Pattern : in String;
                     Within  : in out Text;
                     From    : in Side := Left) is

      Result : constant String := Delete(Pattern,
                                         Within.Value(1..Within.Last),
                                         From);

   begin
      Within.Value(Result'Length + 1..Within.Last) := (others => Null_Character);
      Within.Last := Result'Length;
      Within.Value(1..Result'Length) := Result;
   end Delete;

   procedure Delete (Pattern : in Text;
                     Within  : in out Text;
                     From    : in Side := Left) is

      Result : constant String := Delete(Pattern.Value(1..Pattern.Last),
                                         Within.Value(1..Within.Last),
                                         From);

   begin
      Within.Value(Result'Length + 1..Within.Last) := (others => Null_Character);
      Within.Last := Result'Length;
      Within.Value(1..Result'Length) := Result;
   end Delete;


   function "-" (T : Text; Pattern : Character) return Text is

      Result : constant String := "-"(T.Value(1..T.Last), Pattern);

   begin
      return Text'(Max_Length |
                   Last       => Result'Length,
                   Value      => Result);
   end "-";

   function "-" (T : Text; Pattern : String) return Text is

      Result : constant String := "-"(T.Value(1..T.Last), Pattern);

   begin
      return Text'(Max_Length |
                   Last       => Result'Length,
                   Value      => Result);
   end "-";

   function "-" (T : Text; Pattern : Text) return Text is

      Result : constant String := "-"(T.Value(1..T.Last),
                                      Pattern.Value(1..Pattern.Last));

   begin
      return Text'(Max_Length |
                   Last       => Result'Length,
                   Value      => Result);
   end "-";


   package body Generic_Varying_Text_IO is
   ------------------------------------

      procedure Put (Item : in Text) is
      begin
         Put(File => Current_Output, Item => Item);
      end;

      procedure Put (File : in File_Type; Item : in Text) is
      begin
         Put(File, Item.Value(1..Item.Last));
      end;


      procedure Put_Line (Item : in Text) is
      begin
         Put_Line(File => Current_Output, Item => Item);
      end;

      procedure Put_Line (File : in File_Type; Item : in Text) is
      begin
         Put_Line(File, Item.Value(1..Item.Last));
      end;


      procedure Get (Item : out Text; Width : Field := 0) is
      begin
         Get(File => Current_Input, Item => Item, Width => Width);
      end Get;

      procedure Get (File : in File_Type; Item : out Text; Width : Field := 0) is

         Max : Index;
         I   : Index := 0;

      begin
         if Width = 0 then
            Max := Item.Max_Length;
         else
            Max := Index(Width);
         end if;
         while not End_Of_Line(File) and I < Max loop
            I := I + 1;
            Get(File, Item.Value(I));
         end loop;
         Item.Last := I;
         Item.Value(I + 1..Item.Max_Length) := (others => Null_Character);
      end Get;


      procedure Get_Line (Item : out Text) is
      begin
         Get_Line(File => Current_Input, Item => Item);
      end Get_Line;

      procedure Get_Line (File : in File_Type; Item : out Text) is

         Length : Index;

      begin
         Get_Line(File, Item.Value, Last => Length);
         Item.Last := Length;
         Item.Value(Length + 1..Item.Max_Length) := (others => Null_Character);
         if Length = Item.Max_Length then
            if End_Of_Line(File) then
               Skip_Line(File);
            else
               raise Length_Error;
            end if;
         end if;
      end Get_Line;

   end Generic_Varying_Text_IO;

end Generic_Varying_Text;
