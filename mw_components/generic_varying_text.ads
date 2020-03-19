-- GENERIC TEXT HANDLING PACKAGE
   -----------------------------

-- Revision : 16-NOV-1989 by Mats Weber, removed Upper_Case and Lower_Case.
--                                       Added generic procedure Transformation.
-- Revision : 21-FEB-1989 by Mats Weber, made generic formal type CHARACTER private instead of (<>).
-- Revision : 25-APR-1988 by Mats Weber, replaced FIRST and LAST parameters in ASSIGN procedures
--                                       with a single POSITION parameter, and made "=" (TEXT, TEXT) work
--                                       properly with TEXTs having the same maximum length (this is necessary
--                                       for comparing records having components of type TEXT).
-- Revision : 17-APR-1988 by Mats Weber, added ASSIGN procedures to modify an individual character
--                                       or a slice of a TEXT variable.
-- Revision : 11-APR-1988 by Mats Weber, moved the input-output procedures to the generic
--                                       subpackage GENERIC_VARYING_TEXT_IO,
--                                       removed the generic formal PUT and GET procedures
--                                       without the FILE_TYPE parameter and added generic formal
--                                       function CURRENT_INPUT.
-- Revision :  7-APR-1988 by Mats Weber, made generic.
-- Revision : 11-MAR-1988 by Mats Weber, removed derived type SIDE.
-- Revision : 21-FEB-1988 by Mats Weber, added generic function SCAN.
-- Revision :  9-DEC-1987 by Mats Weber, added function LOCATED.
-- Revision : 29-MAR-1987 by Mats Weber, made type TEXT non limited and renamed
--                                       function "=" to EQUAL.
-- Revision :  8-JUL-1986 by Mats Weber, added procedures PUT and GET.

-- Creation : 28-JUN-1986 by Mats Weber


generic
   type Character is private;
   Null_Character : in Character;  -- Character used to fill unused positions in TEXT objects.

   type String is array (Positive range <>) of Character;

   with function "<"  (Left, Right : String) return Boolean is <>;
   with function ">"  (Left, Right : String) return Boolean is <>;
   with function "<=" (Left, Right : String) return Boolean is <>;
   with function ">=" (Left, Right : String) return Boolean is <>;

   type Side is (<>);       -- must have two values named Left and Right

   with function Left  return Side is <>;
   with function Right return Side is <>;

   with function Locate (Pattern : Character; Within : String; From : Side) return Natural is <>;
   with function Locate (Pattern : String;    Within : String; From : Side) return Natural is <>;
   with function Located (Pattern : Character; Within : String) return Boolean is <>;
   with function Located (Pattern : String;    Within : String) return Boolean is <>;
   with function Delete (Pattern : Character; Within : String; From : Side) return String is <>;
   with function Delete (Pattern : String;    Within : String; From : Side) return String is <>;
   with function "-" (Left : String; Right : Character) return String is <>;
   with function "-" (Left : String; Right : String)    return String is <>;
package Generic_Varying_Text is
----------------------------

   subtype Index is Natural;

   type Text (Max_Length : Index) is private;
   ------------------------------

      -- The function "=" (Text, Text) will return True if and only if both
      -- arguments have the same maximum length and the same contents.


   Null_Text    : constant Text;

   Index_Error  : exception;    -- Raised when an attempt is made to access a character
                                -- with an index outside the range 1..Length.

   Length_Error : exception;    -- Raised when an attempt is made to make an object of
                                -- type Text longer than its maximum length.

                                -- If both error conditions are present, Index_Error is raised.


   function Length    (Of_Text  : Text) return Index;
   function Empty     (The_Text : Text) return Boolean;
   function To_String (The_Text : Text) return String;
   function Char      (Of_Text  : Text; Position : Index) return Character;


   function To_Text (Value : Character) return Text;
   function To_Text (Value : String)    return Text;
      -- The maximum length of the result is Value'Length

   function To_Text (Value : Character; Max_Length : Index) return Text;
   function To_Text (Value : String;    Max_Length : Index) return Text;
      -- The maximum length of the result is Max_Length


   procedure Assign (Object : out Text; Value : in Character);
   procedure Assign (Object : out Text; Value : in String);
   procedure Assign (Object : out Text; Value : in Text);
      -- Object := Value

   procedure Assign (Object : in out Text; Position : in Index; Value : in Character);
      -- Object(Position) := Value
      -- The length of Object may grow as a result of a call to this procedure.

   procedure Assign (Object : in out Text; Position : in Index; Value : in String);
   procedure Assign (Object : in out Text; Position : in Index; Value : in Text);
      -- Object(Position..Position + Length(Value) - 1) := Value
      -- The length of Object may grow as a result of a call to these procedures.


   function "&" (T : Text;      U : Text)      return Text;
   function "&" (T : Text;      U : String)    return Text;
   function "&" (T : String;    U : Text)      return Text;
   function "&" (T : Text;      U : Character) return Text;
   function "&" (T : Character; U : Text)      return Text;

   function Equal (T : Text;   U : Text)   return Boolean;
   function Equal (T : Text;   U : String) return Boolean;
   function Equal (T : String; U : Text)   return Boolean;

   function "<"  (T, U : Text) return Boolean;
   function ">"  (T, U : Text) return Boolean;
   function "<=" (T, U : Text) return Boolean;
   function ">=" (T, U : Text) return Boolean;


   generic
      with function Condition (The_Character : Character) return Boolean;
   function Scan (Within : Text;
                  From   : Side := Left) return Index;
      -- Returns the position of the first character for which Condition
      -- returns True.

   generic
      with procedure Transform (The_Character : in out Character);
   procedure Transformation (On_Text : in out Text);
      -- Transforms On_Text by calling Transform for each of its
      -- characters.


   function Locate (Pattern : Character;
                    Within  : Text;
                    From    : Side := Left) return Index;

   function Locate (Pattern : String;
                    Within  : Text;
                    From    : Side := Left) return Index;

   function Locate (Pattern : Text;
                    Within  : Text;
                    From    : Side := Left) return Index;
      -- Returns the position of the first character of the first occurence
      -- of Pattern in Within. If Pattern is not found, then 0 is returned
      -- when From = Right, or Length(Within) + 1 when From = Left.
      -- From indicates at which end of Within the search must begin.


   function Located (Pattern : Character; Within : Text) return Boolean;
   function Located (Pattern : String;    Within : Text) return Boolean;
   function Located (Pattern : Text;      Within : Text) return Boolean;
      -- Returns True if and only if Pattern is a substring of Within.

   function Substring (Of_Text : Text; First, Last : Index) return Text;
   function Substring (Of_Text : Text; First, Last : Index) return String;
      -- Returns Of_Text(First..Last).

   procedure Insert (Fragment : in Character;
                     Into     : in out Text;
                     Position : in Index);

   procedure Insert (Fragment : in String;
                     Into     : in out Text;
                     Position : in Index);

   procedure Insert (Fragment : in Text;
                     Into     : in out Text;
                     Position : in Index);
      -- Inserts Fragment at the specified position.

   procedure Append (Fragment : in Character; To : in out Text);
   procedure Append (Fragment : in String;    To : in out Text);
   procedure Append (Fragment : in Text;      To : in out Text);
      -- Appends Fragment to To.

   procedure Truncate (The_Text : in out Text; Length : in Index);
      -- Truncates The_Text to the given length.

   procedure Delete (Within : in out Text; First, Length : in Index);
      -- Deletes Length characters from Within, beginning at First.


   procedure Delete (Pattern : in Character;
                     Within  : in out Text;
                     From    : in Side := Left);

   procedure Delete (Pattern : in String;
                     Within  : in out Text;
                     From    : in Side := Left);

   procedure Delete (Pattern : in Text;
                     Within  : in out Text;
                     From    : in Side := Left);
      -- Removes the first occurence of Pattern in within.


   function "-" (T : Text; Pattern : Character) return Text;
   function "-" (T : Text; Pattern : String)    return Text;
   function "-" (T : Text; Pattern : Text)      return Text;
      -- Returns T with all occurences of Pattern removed.


   -- Input/output procedures

   generic
      type File_Type is limited private;
      type Field is range <>;
      type Count is range <>;

      with procedure Put      (File : in File_Type; Item : in String) is <>;
      with procedure Put_Line (File : in File_Type; Item : in String) is <>;
      with procedure Get      (File : in File_Type; Item : out Character) is <>;
      with procedure Get_Line (File : in File_Type; Item : out String; Last : out Natural) is <>;
      with procedure Skip_Line (File : in File_Type; Spacing : in Count := 1) is <>;
      with function End_Of_Line (File : in File_Type) return Boolean is <>;
      with function Current_Output return File_Type is <>;
      with function Current_Input  return File_Type is <>;
   package Generic_Varying_Text_IO is
   -------------------------------

      procedure Put (Item : in Text);
      procedure Put (File : in File_Type; Item : in Text);
         -- equivalent to Put(To_String(Item));

      procedure Put_Line (Item : in Text);
      procedure Put_Line (File : in File_Type; Item : in Text);
         -- equivalent to Put_Line(To_String(Item));

      procedure Get (Item : out Text; Width : Field := 0);
      procedure Get (File : in File_Type; Item : out Text; Width : Field := 0);
         -- if Width is 0, then characters are read until the end of line
         -- is reached or Item.Max_Length characters have been read;
         -- if Width is not 0, then exactly Width characters are read unless
         -- the end of line is reached.
         -- Skip_Line is not called.

      procedure Get_Line (Item : out Text);
      procedure Get_Line (File : in File_Type; Item : out Text);
         -- reads to the end of the line and calls Skip_Line.
         -- Length_Error will be raised if Item is too short.


      pragma Inline(Put, Put_Line);

   end Generic_Varying_Text_IO;


private

   type Text (Max_Length : Index) is
      record
         Last  : Index := 0;
         Value : String(1..Max_Length) := (others => Null_Character);  -- default value to allow the use
      end record;                                                      -- of "=" (Text, Text).

   Null_Text : constant Text := (Max_Length |
                                 Last       => 0,
                                 Value      => (others => Null_Character));


   pragma Inline(Length, Empty, To_String, Char,
                 Assign, Equal, "<", ">", "<=", ">=",
                 Scan, Transformation,
                 Locate, Located, Substring);

end Generic_Varying_Text;
