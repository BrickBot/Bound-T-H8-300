-- LEXICAL ANALYZER FOR THE ADA LANGUAGE
   -------------------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Wed Nov  5 13:51:10 1997
-- Update Count    : 1

-- Creation : 16-SEP-1988 by Mats Weber.


with Binary_Search,
     String_Case_Conversions;

use String_Case_Conversions;

separate (Ada_Lexical_Analyzer)

package body Ada_Keywords is
-------------------------

   type String_Access is access String;

   type String_Access_Array is array (Positive range <>) of String_Access;

   function Value (A : String_Access) return String;

   package String_Binary_Search is new Binary_Search(Key_Type   => String,
                                                     Item_Type  => String_Access,
                                                     Key_Of     => Value,
                                                     Index      => Positive,
                                                     Item_Array => String_Access_Array);

   use String_Binary_Search;

   All_Keywords : constant String_Access_Array :=
      (new String'("abort"),
       new String'("abs"),
       new String'("abstract"),
       new String'("accept"),
       new String'("access"),
       new String'("aliased"),
       new String'("all"),
       new String'("and"),
       new String'("array"),
       new String'("at"),
       new String'("begin"),
       new String'("body"),
       new String'("case"),
       new String'("constant"),
       new String'("declare"),
       new String'("delay"),
       new String'("delta"),
       new String'("digits"),
       new String'("do"),
       new String'("else"),
       new String'("elsif"),
       new String'("end"),
       new String'("entry"),
       new String'("exception"),
       new String'("exit"),
       new String'("for"),
       new String'("function"),
       new String'("generic"),
       new String'("goto"),
       new String'("if"),
       new String'("in"),
       new String'("is"),
       new String'("limited"),
       new String'("loop"),
       new String'("mod"),
       new String'("new"),
       new String'("not"),
       new String'("null"),
       new String'("of"),
       new String'("or"),
       new String'("others"),
       new String'("out"),
       new String'("package"),
       new String'("pragma"),
       new String'("private"),
       new String'("procedure"),
       new String'("protected"),
       new String'("raise"),
       new String'("range"),
       new String'("record"),
       new String'("rem"),
       new String'("renames"),
       new String'("requeue"),
       new String'("return"),
       new String'("reverse"),
       new String'("select"),
       new String'("separate"),
       new String'("subtype"),
       new String'("tagged"),
       new String'("task"),
       new String'("terminate"),
       new String'("then"),
       new String'("type"),
       new String'("until"),
       new String'("use"),
       new String'("when"),
       new String'("while"),
       new String'("with"),
       new String'("xor"));


   function Value (A : String_Access) return String is
   begin
      return A.all;
   end;


   function Is_Keyword (Word : String) return Boolean is
   begin
      return Locate(Key    => Lower_Case(Word),
                    Within => All_Keywords).Kind = Found;
   end Is_Keyword;

end Ada_Keywords;
