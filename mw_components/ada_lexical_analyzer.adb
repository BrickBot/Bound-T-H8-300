-- LEXICAL ANALYZER FOR THE ADA LANGUAGE
   -------------------------------------

-- Creation : 29-APR-1988 by Mats Weber.


package body Ada_Lexical_Analyzer is
---------------------------------

   package Ada_Keywords is
   --------------------

      function Is_Keyword (Word : String) return Boolean;

   end Ada_Keywords;

   package body Ada_Keywords is separate;


   package body Analyzer is separate;
   ---------------------

end Ada_Lexical_Analyzer;
