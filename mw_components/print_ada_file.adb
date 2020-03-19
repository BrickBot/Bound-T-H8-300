-- PROGRAM TO MAKE RESERVED WORDS BOLD IN ADA SOURCE FILES
   -------------------------------------------------------

-- Creation : 26-SEP-1988 by Mats Weber.


with The_Ada_Lexical_Analyzer,
     Number_Images,
     Text_IO,
     Text_IO_Streams,
     String_Handler,
     Varying_Text,
     Varying_Text_IO,
     User_Interface,
     Command_Line;

use The_Ada_Lexical_Analyzer,
    String_Handler,
    Varying_Text,
    Varying_Text_IO,
    Text_IO,
    Command_Line;

procedure Print_Ada_File is
------------------------

   N_Files_Processed  : Natural := 0;

   Print_Line_Numbers : constant Boolean := User_Interface.Yes_No_Answer("Print Line Numbers ? ");
   Italicize_Comments : constant Boolean := User_Interface.Yes_No_Answer("Italicize Comments ? ");


   procedure Print_Ada_File (Source_Name : in String) is

      Source,
      Destination       : Text_IO.File_Type;

      package Source_Stream is new Text_IO_Streams.Input_Stream(File => Source);

      package The_Analyzer is
         new The_Ada_Lexical_Analyzer.Analyzer(Get                        => Source_Stream.Get,
                                               Include_Separators         => True,
                                               Allow_Alternate_Characters => True);

      Lex               : Lexical_Element;

      Normal_Sequence   : constant String := ASCII.Esc & "[0m";
      Bold_Sequence     : constant String := ASCII.Esc & "[1m";
      Italic_Sequence   : constant String := ASCII.Esc & "[3m";

      Line_Number       : Text_IO.Count := 0;


      function Image is new Number_Images.Integer_Image(Text_IO.Positive_Count);


      procedure Put_Line_Number is
      begin
         if Print_Line_Numbers then
            Line_Number := Line_Number + 1;
            Put(Destination, Image(Line_Number, Width => 6) & "  ");
         end if;
      end Put_Line_Number;

   begin
      Put_Line(Source_Name);
      Open(Source, Name => Source_Name, Mode => In_File);
      Create(File => Destination,
             Name => Source_Name(Source_Name'First ..
                                 Locate(".a", Source_Name, From => Right) - 1) &
                     ".print");
      Put_Line_Number;
      loop
         The_Analyzer.Get_Next_Lexical_Element(Lex);
         case Lex.Kind is
            when Separators =>
               for I in 1..Length(Lex.Image) loop
                  case Char(Lex.Image, Position => I) is
                     when ' ' | ASCII.HT | ASCII.VT =>
                        Put(Destination, Char(Lex.Image, Position => I));
                     when Line_Separator =>
                        New_Line(Destination);
                        Put_Line_Number;
                     when Page_Separator =>
                        New_Page(Destination);
                        Put_Line_Number;
                     when others =>
                        Put_Line("   Bizarre separator at page " & Image(Page(Destination)) &
                                 ", line " & Image(Line(Destination)));
                        Put(Destination, Char(Lex.Image, Position => I));
                  end case;
               end loop;
            when Reserved_Word =>
               Put(Destination, Bold_Sequence);
               Put(Destination, Lex.Image);
               Put(Destination, Normal_Sequence);
            when Comment =>
               if Italicize_Comments then
                  Put(Destination, Italic_Sequence);
               end if;
               Put(Destination, Lex.Image);
               if Italicize_Comments then
                  Put(Destination, Normal_Sequence);
               end if;
            when Lexical_Error =>
               Put_Line("   Lexical error at page " & Image(Page(Destination)) &
                        ", line " & Image(Line(Destination)));
               Put(Destination, Lex.Image);
            when End_Of_Source =>
               exit;
            when others =>
               Put(Destination, Lex.Image);
         end case;
      end loop;
      Close(Destination);
      N_Files_Processed := N_Files_Processed + 1;
   end Print_Ada_File;


   function Image is new Number_Images.Integer_Image(Natural);


begin
   for I in 1 .. Command_Line.Argv'Last loop
      Print_Ada_File(Command_Line.Argv(I).S);
   end loop;
   New_Line;
   Put_Line(Image(N_Files_Processed) & " file(s) processed");
end Print_Ada_File;
