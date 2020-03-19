-- LEXICAL ANALYZER FOR THE ADA LANGUAGE
   -------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Aug  4 15:40:03 1998
-- Update Count    : 7

-- Revision : 26-SEP-1989 by Mats Weber, made generic with respect to Next_Character instead
--                                       of Text_IO.
-- Revision :  1-FEB-1989 by Mats Weber, added generic parameter MAX_SOURCE_LINE_LENGTH.
-- Revision : 20-OCT-1988 by Mats Weber, made generic with respect to TEXT_IO.
-- Revision : 19-OCT-1988 by Mats Weber, added formal parameters ALLOW_ALTERNATE_CHARACTERS and
--                                       INCLUDE_COMMENTS to entry ANALYZER.OPEN.
-- Revision : 26-SEP-1988 by Mats Weber, removed TEXT_FILE_POSITION and added SEPARATORS to
--                                       LEXICAL_ELEMENT_KIND instead of returning the position
--                                       of the lexical elements.
-- Revision : 19-MAY-1988 by Mats Weber, removed exceptions LEXICAL_ERROR and END_OF_SOURCE and
--                                       replaced them with values of the type LEXICAL_ELEMENT_KIND.

-- Creation : 29-APR-1988 by Mats Weber.


with Largest_Numeric_Types,
     Text_Stream_Definitions,
     Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package Ada_Lexical_Analyzer is
----------------------------

   subtype Lexical_Element_Image is Ada.Strings.Unbounded.Unbounded_String;

   type Lexical_Element_Kind is (Separators,
                                 Delimiter,
                                 Identifier,
                                 Reserved_Word,
                                 Integer_Literal,
                                 Real_Literal,
                                 Character_Literal,
                                 String_Literal,
                                 Comment,
                                 Lexical_Error,
                                 End_Of_Source);

   Line_Separator   : Character renames Text_Stream_Definitions.Line_Separator;
   Page_Separator   : Character renames Text_Stream_Definitions.Page_Separator;
   File_Terminator  : Character renames Text_Stream_Definitions.File_Terminator;

      -- Separators is any sequence of ' ', ASCII.HT, ASCII.VT, Line_separator and Page_separator.
      -- A Page_Separator means a line terminator followed by a page terminator.
      -- An End_Of_Source means a line terminator followed by a page terminator
      -- followed by a file terminator.


   type Delimiter_Value is ('&', ''', '(', ')',
                            '*', '+', ',', '-',
                            '.', '/', ':', ';',
                            '<', '=', '>', '|',
                            Arrow,                     -- '=>'
                            Double_Dot,                -- '..'
                            Double_Star,               -- '**'
                            Becomes,                   -- ':='
                            Not_Equal,                 -- '/='
                            Greater_Than_Or_Equal,     -- '>='
                            Less_Than_Or_Equal,        -- '<='
                            Left_Label_Bracket,        -- '<<'
                            Right_Label_Bracket,       -- '>>'
                            Box);                      -- '<>'

   subtype Identifier_Value is Ada.Strings.Unbounded.Unbounded_String;
   subtype Reserved_Word_Value is Ada.Strings.Unbounded.Unbounded_String;

   type Integer_Literal_Range is new Largest_Numeric_Types.Large_Integer;
   type Real_Literal_Digits is new Largest_Numeric_Types.Large_Float;

   type Integer_Literal_Value (Valid : Boolean := False) is
      record
         case Valid is
            when True  => Value : Integer_Literal_Range;
            when False => null;
         end case;
      end record;

   type Real_Literal_Value (Valid : Boolean := False) is
      record
         case Valid is
            when True  => Value : Real_Literal_Digits;
            when False => null;
         end case;
      end record;

   subtype Character_Literal_Value is Character;
   subtype String_Literal_Value is Ada.Strings.Unbounded.Unbounded_String;

   type Comment_Value is new Ada.Strings.Unbounded.Unbounded_String;


   type Lexical_Element (Kind : Lexical_Element_Kind := Separators) is
      record
         Image : Lexical_Element_Image;     -- the lexical element as it appears in the source file
         case Kind is
            when Delimiter         => The_Delimiter         : Delimiter_Value;
            when Identifier        => The_Identifier        : Identifier_Value;
                                         -- always returned in upper case
            when Reserved_Word     => The_Reserved_Word     : Reserved_Word_Value;
                                         -- always returned in lower case
            when Integer_Literal   => The_Integer_Literal   : Integer_Literal_Value;
            when Real_Literal      => The_Real_Literal      : Real_Literal_Value;
            when Character_Literal => The_Character_Literal : Character_Literal_Value;
            when String_Literal    => The_String_Literal    : String_Literal_Value;
            when Comment           => The_Comment           : Comment_Value;
            when Separators |
                 Lexical_Error |
                 End_Of_Source     => null;
         end case;
      end record;


   generic
      with procedure Get (Item : out Character);
         -- Must return all characters in the source file in sequence.
         -- Line_Separator must be returned to separate lines,
         -- Page_Separator must be returned to separate pages
         --    (without any preceeding Line_Separator, except for blank lines),
         -- File_Terminator must be returned at the end of the file
         --    (without any preceeding Line_Separator or Page_Separator,
         --     except for blank lines or pages).

      Include_Separators         : in Boolean := False;
      Include_Comments           : in Boolean := True;
      Allow_Alternate_Characters : in Boolean := False;
         -- If Allow_Alternate_Characters is True, then the analyzer will allow
         -- '!', ':' and '%' instead of '|', '#' and '"' respectively as specified
         -- in LRM 2.10.

   package Analyzer is
   ----------------

      procedure Reset;
         -- Resets the analyzer
         -- (called automatically when an instance of Analyzer is elaborated).

      procedure Get_Next_Lexical_Element (Element : out Lexical_Element);
         -- Will raise Source_At_End if the entire source file has
         -- been consumed and a lexical element with Kind = End_Of_Source
         -- has already been returned.

   end Analyzer;


   Source_At_End : exception;

end Ada_Lexical_Analyzer;
