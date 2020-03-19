with Text_IO;

separate (Distributor.Network_Handler)

procedure Read_Nodes_File (File_Name : in String;
                           Into      : in out Node_Tables.Table) is

   Nodes_File      : Text_IO.File_Type;
   Current_Line    : String(1..300);
   Length          : Natural range 0..Current_Line'Last;
   New_Descriptor  : Node_Descriptor;
   New_Node        : Node_Id := Root_Node;

   function Blank (The_Character : in Character) return Boolean is
   begin
      return The_Character = ' ' or The_Character = ASCII.HT;
   end;

begin
   Text_IO.Open(File => Nodes_File,
                Name => File_Name,
                Mode => Text_IO.In_File);
   Node_Tables.Destroy(The_Table => Into);
   while not Text_IO.End_Of_File(Nodes_File) loop
      Text_IO.Get_Line(File => Nodes_File,
                       Item => Current_Line,
                       Last => Length);
      declare

         I : Positive range 1..Length + 1 := 1;

         procedure Get (Word : out Varying_Text.Text) is

            New_Word : Varying_Text.Text(Word.Max_Length);

         begin
            Varying_Text.Assign(New_Word, Value => "");
            while I <= Length and then Blank(Current_Line(I)) loop
               I := I + 1;
            end loop;
            while I <= Length and then not Blank(Current_Line(I)) loop
               Varying_Text.Append(Current_Line(I), To => New_Word);
               I := I + 1;
            end loop;
            while I <= Length and then Blank(Current_Line(I)) loop
               I := I + 1;
            end loop;
            Varying_Text.Assign(Word, Value => New_Word);
         end Get;

      begin
         Get(Word => New_Descriptor.Node_Name);
         if I <= Length then
            Get(Word => New_Descriptor.Object_Name);
            if I <= Length then
               -- the current line contains more than two words
               raise Error_In_Nodes_File;
            end if;
            New_Node := New_Node + 1;
            Node_Tables.Insert(Key  => New_Node,
                               Item => New_Descriptor,
                               Into => Into);
         elsif Varying_Text.Length(New_Descriptor.Node_Name) > 0 then
            -- the current line contains only one word
            raise Error_In_Nodes_File;
         end if;
         -- blank lines are accepted and skipped
      end;
   end loop;
   Text_IO.Close(File => Nodes_File);
exception
   when others =>
      if Text_IO.Is_Open(File => Nodes_File) then
         Text_IO.Close(File => Nodes_File);
      end if;
      raise;
end Read_Nodes_File;
