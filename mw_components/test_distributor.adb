-- TEST PROGRAM FOR DISTRIBUTOR
   ----------------------------

-- Creation :  4-DEC-1989 by Mats Weber.


with Distributor,
     Text_IO,
     Number_Images,
     User_Interface,
     Calendar,
     Min_Max_Functions,
     Random,
     Random_Numeric_Types,
     Unchecked_Conversion,
     Unchecked_Deallocation,
     Tables,
     VMS_System,
     Varying_Text;

use Calendar,
    Text_IO,
    Varying_Text;

procedure Test_Distributor is
--------------------------

   pragma Time_Slice(0.02);

   Test_Error : exception;

   function Natural_Answer  is new User_Interface.Integer_Answer(Natural);
   function Duration_Answer is new User_Interface.Fixed_Answer(Duration);


   Noise_Message_Size : constant Natural := Natural_Answer("Size of noise messages in bytes : ");

   type Byte is range -128..127;

   for Byte'Size use 8;

   type Noise is array (1..Noise_Message_Size) of Byte;

   function Uniform_Noise is new Random.Enumeration_Uniform(Byte);

   The_Noise : Noise;

   type Test_Message_Kind is (Hello, Noise_Out, Noise_In);

   type Hello_Message is
      record
         Number_Of_Processes_To_Create : Natural;
         Stop_Time                     : Calendar.Time;
         Noise_Out_Delay               : Duration;
         Number_Of_Available_Nodes     : Natural;
         Max_Termination_Delay         : Duration;
         Terminate_On_Process_Failure  : Boolean;
      end record;

   type Test_Message (Kind : Test_Message_Kind := Noise_Out) is
      record
         case Kind is
            when Hello =>
               Hello_Part    : Hello_Message;
            when Noise_Out | Noise_In =>
               Time_Sent     : Calendar.Time;
               Noise_Part    : Noise;
               Serial_Number : Natural;
         end case;
      end record;

   package Test_Message_Distributor is new Distributor(Test_Message);

   use Test_Message_Distributor;

   Son_1, Son_2, Son_3     : Process_Id;
   Message_1               : Message;
   Sender_1                : Process_Id;

   Start_Time,
   Stop_Time               : Calendar.Time;
   Startup_Done            : Boolean := False;

   Hi                      : Hello_Message;
   Hello_Gotten            : Boolean := False;
   Number_Of_Processes     : Natural := 0;
   Out_Delay               : Duration;
   Process_Failure_Gotten  : Boolean := False;


   task type Noise_Out_Sender_Task is
      entry Give_Noise (Out_Noise : in Noise);
      entry Send (To : in Process_Id; After : in Duration; Number : in Natural);
      entry Stop;
   end Noise_Out_Sender_Task;

   type Noise_Out_Sender_Access is access Noise_Out_Sender_Task;

   type Process_Information (Average_Started : Boolean := False) is
      record
         Message_Count      : Natural;
         Noise_Out_Sender   : Noise_Out_Sender_Access;
         Out_Noise          : Noise;
         Last_Serial_Number : Natural;
         case Average_Started is
            when False =>
               null;
            when True =>
               Average_Count : Natural;
               Average_Sum   : Float;
               Min, Max      : Duration;
         end case;
      end record;

   package Process_Tables is new Tables(Key_Type  => Process_Id,
                                        Item_Type => Process_Information,
                                        Count     => Natural);

   use Process_Tables;

   Known_Processes            : Process_Tables.Table;
   Noise_Out_Senders_Stopped  : Boolean := False;


   function Image is new Number_Images.Integer_Image(Natural);
   function Image is new Number_Images.Float_Image(Float, Default_Fore => 2, Default_Aft => 3);
   function Image is new Number_Images.Fixed_Image(Duration, Default_Fore => 2, Default_Aft => 3);


   task body Noise_Out_Sender_Task is

      Destination  : Process_Id;
      Send_Delay   : Duration;
      The_Noise    : Noise;
      The_Number   : Natural;

   begin
      accept Give_Noise (Out_Noise : in Noise) do
         The_Noise := Out_Noise;
      end;
      loop
         select
            accept Send (To : in Process_Id; After : in Duration; Number : in Natural) do
               Destination := To;
               Send_Delay  := After;
               The_Number  := Number;
            end Send;
            select
               accept Stop;
               exit;
            or
               delay Send_Delay;
            end select;
            Send((Kind          => Noise_Out,
                  Time_Sent     => Calendar.Clock,
                  Noise_Part    => The_Noise,
                  Serial_Number => The_Number),
                 To => Destination);
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;
      end loop;
   end Noise_Out_Sender_Task;


   function Image (Process : Process_Id) return String is

      Max_Number_Of_Sons : constant := 100;
      Max_Tree_Depth     : constant := 50;

      type Son_Number is range  0..Max_Number_Of_Sons;
      type Path_Index is range -1..Max_Tree_Depth;

      subtype Positive_Son_Number is Son_Number range 1..Son_Number'Last;
      subtype Positive_Path_Index is Path_Index range 1..Path_Index'Last;

      type Process_Path is
         array (Positive_Path_Index range <>) of Positive_Son_Number;

      type Variable_Process_Path (Depth : Path_Index := 0) is
         record
            Path : Process_Path(1..Depth);
         end record;

      type Process_Name is
         record
            Dewey : Variable_Process_Path;
         end record;

      function To_Process_Name is new Unchecked_Conversion(Process_Id,
                                                           Process_Name);

      Result : Text(100);

      Name   : constant Process_Name := To_Process_Name(Process);


      function Image is new Number_Images.Integer_Image(Son_Number);
      function Image is new Number_Images.Integer_Image(Path_Index);

   begin
      Append('(', To => Result);
      for I in Name.Dewey.Path'Range loop
         Append(Image(Name.Dewey.Path(I)), To => Result);
         exit when I = Name.Dewey.Path'Last;
         Append('.', To => Result);
      end loop;
      Append(')', To => Result);
      return To_String(Result);
   end Image;


   function Index_Of (Node : Node_Id) return Natural is
   begin
      if Node = Root_Node then
         return 0;
      end if;
      declare

         Nodes : constant Node_Id_List := Available_Nodes;

      begin
         for I in Nodes'Range loop
            if Node = Nodes(I) then
               return I;
            end if;
         end loop;
      end;
      Put_Line("!!! Unknown Node_Id");
      raise Test_Error;
   end Index_Of;


   function Image (Node : Node_Id) return String is
   begin
      return Image(Index_Of(Node));
   end;


   procedure Set_Process_Name (Phase : in String) is
   begin
      VMS_System.Set_Process_Name(To => Image(Current_Process) & Phase);
   exception
      when others =>
         for Letter in 'A'..'Z' loop
            begin
               VMS_System.Set_Process_Name(To => "(..." & Letter & "...)" & Phase);
               exit;
            exception
               when others =>
                  null;
            end;
         end loop;
   end Set_Process_Name;


   function Random_Node return Node_Id is

      subtype Node_Number is Positive range Available_Nodes'Range;

      function Uniform is new Random.Enumeration_Uniform(Node_Number);

   begin
      return Available_Nodes(Uniform);
   end Random_Node;


   procedure Incr_Message_Count (Process : in Process_Id) is

      procedure Incr (Info : in out Process_Information) is
      begin
         Info.Message_Count := Info.Message_Count + 1;
      end Incr;

      procedure Add_One is new Process_Tables.Update(Modify => Incr);

   begin
      Add_One(Key => Process, Within => Known_Processes);
   exception
      when Process_Tables.Nonexistent_Key =>
         declare

            New_Noise_Out_Sender  : constant Noise_Out_Sender_Access := new Noise_Out_Sender_Task;
            New_Out_Noise         : constant Noise := (others => Uniform_Noise);

         begin
            New_Noise_Out_Sender.Give_Noise(Out_Noise => New_Out_Noise);
            Insert(Key  => Process,
                   Item => (Message_Count      => 1,
                            Average_Started    => False,
                            Noise_Out_Sender   => New_Noise_Out_Sender,
                            Out_Noise          => New_Out_Noise,
                            Last_Serial_Number => 0),
                   Into => Known_Processes);
         end;
   end Incr_Message_Count;


   procedure Average_Time (Process            : in Process_Id;
                           Time               : in Duration;
                           Noise_Back         : in Noise;
                           Serial_Number_Back : in Natural) is

      procedure Add_To_Sum (Info : in out Process_Information) is

         function Min is new Min_Max_Functions.Minimum(Duration);
         function Max is new Min_Max_Functions.Maximum(Duration);

      begin
         if Info.Average_Started then
            Info.Average_Count := Info.Average_Count + 1;
            Info.Average_Sum   := Info.Average_Sum + Float(Time);
            Info.Max           := Max(Info.Max, Time);
            Info.Min           := Min(Info.Min, Time);
         else
            Info := (Average_Started    => True,
                     Message_Count      => Info.Message_Count,
                     Noise_Out_Sender   => Info.Noise_Out_Sender,
                     Out_Noise          => Info.Out_Noise,
                     Last_Serial_Number => Info.Last_Serial_Number,
                     Average_Count      => 0,                     -- Do not count the first message
                     Average_Sum        => 0.0,                   -- in the average
                     Min                => Duration'Last,
                     Max                => Duration'First);
         end if;
         if Serial_Number_Back /= Info.Last_Serial_Number then
            Put_Line("!!! Wrong serial number came back from " & Image(Process));
            raise Test_Error;
         end if;
         if (Serial_Number_Back = 0 and then Noise_Back /= The_Noise) or
            (Serial_Number_Back > 0 and then Noise_Back /= Info.Out_Noise)
         then
            Put_Line("!!! Wrong noise came back from " & Image(Process));
            raise Test_Error;
         end if;
         Info.Last_Serial_Number := Info.Last_Serial_Number + 1;
      end Add_To_Sum;

      procedure Do_Add_To_Sum is new Process_Tables.Update(Modify => Add_To_Sum);

   begin
      Do_Add_To_Sum(Key => Process, Within => Known_Processes);
   exception
      when Process_Tables.Nonexistent_Key =>
         Put_Line("!!! Average_Time on an unknown process");
         raise Test_Error;
   end Average_Time;


   procedure Put_Message_Counts is

      procedure Put_Message_Count (Process : in Process_Id;
                                   Info    : in Process_Information) is
      begin
         Put(Image(Process));
         Set_Col(To => 16);
         Put(Image(Info.Message_Count, Width => 6) & " received");
         if Info.Average_Started and then Info.Average_Count /= 0 then
            Put(", " & Image(Info.Average_Sum / Float(Info.Average_Count)) &
                " average on " & Image(Info.Average_Count, Width => 6) &
                ", " & Image(Info.Min) & " min, " & Image(Info.Max) & " max");
         end if;
         New_Line;
      end Put_Message_Count;

      procedure Do_Put_Message_Counts is
         new Process_Tables.Traversal(Action => Put_Message_Count);

   begin
      Do_Put_Message_Counts(On_Table => Known_Processes);
   end Put_Message_Counts;


   procedure Stop_Noise_Out_Senders is

      procedure Stop_Noise_Out_Sender (Process : in Process_Id;
                                       Info    : in out Process_Information) is

         procedure Dispose is new Unchecked_Deallocation(Noise_Out_Sender_Task,
                                                         Noise_Out_Sender_Access);

      begin
         Info.Noise_Out_Sender.Stop;
         Dispose(Info.Noise_Out_Sender);
      end Stop_Noise_Out_Sender;

      procedure Do_Stop_Noise_Out_Senders is
         new Process_Tables.Update_All(Update_Item => Stop_Noise_Out_Sender);

   begin
      Do_Stop_Noise_Out_Senders(Within => Known_Processes);
   end Stop_Noise_Out_Senders;


begin
   if Root_Process = Null_Process or Root_Node = Null_Node then
      Put_Line("!!! Error in Distributor constants");
      raise Test_Error;
   end if;
   Random.Randomize;
   The_Noise := (others => Uniform_Noise);
   Put_Line("I am " & Image(Current_Process) & " on node " & Image(Current_Node));
   Set_Process_Name(Phase => "S");
   if Creator_Process = Null_Process then
      Start_Application(Nodes_File_Name => "Node_Names.Dat");
      if Current_Node /= Root_Node or Current_Process /= Root_Process then
         Put_Line("!!! Error in Current_Node or Current_Process");
         raise Test_Error;
      end if;
      Hi.Number_Of_Processes_To_Create := Natural_Answer("Number Of Processes To Create : ");
      Hi.Stop_Time                     := Calendar.Clock + Duration_Answer("Total run time in seconds : ");
      Hi.Noise_Out_Delay               := Duration_Answer("Mean delay for sending Noise_Out messages : ");
      Hi.Max_Termination_Delay         := Duration_Answer("Maximum termination delay : ");
      Hi.Number_Of_Available_Nodes     := Available_Nodes'Length;
      Hi.Terminate_On_Process_Failure  := User_Interface.Yes_No_Answer("Terminate on process failure ? ");
      Hello_Gotten := True;
      if Available_Nodes'First /= 1 then
         Put_Line("!!! Error in Available_Nodes");
         raise Test_Error;
      end if;
      Create_Process(On_Node     => Random_Node,
                     New_Process => Son_1);
      Put_Line("--- Just created " & Image(Son_1));
      Send((Hello, Hi), To => Son_1);
      if Random.Probability(0.5) then
         Create_Process(On_Node     => Random_Node,
                        New_Process => Son_2);
         Put_Line("--- Just created " & Image(Son_2));
         Send((Hello, Hi), To => Son_2);
      end if;
      if Random.Probability(0.5) then
         Create_Process(On_Node     => Random_Node,
                        New_Process => Son_3);
         Put_Line("--- Just created " & Image(Son_3));
         Send((Hello, Hi), To => Son_3);
      end if;
   else
      if Current_Node = Root_Node or Current_Process = Root_Process then
         Put_Line("!!! Error in Current_Node or Current_Process");
         raise Test_Error;
      end if;
   end if;
   if Current_Node = Null_Node or Current_Process = Null_Process then
      Put_Line("!!! Error in Current_Node or Current_Process");
      raise Test_Error;
   end if;
   if Creator_Process = Current_Process then
      Put_Line("!!! Error in Creator_Process or Current_Process");
      raise Test_Error;
   end if;
   begin
      Start_Application(Nodes_File_Name => "XXX.XXX");
      Put_Line("!!! Application_Already_Started should have been raised");
      raise Test_Error;
   exception
      when Application_Already_Started => null;
   end;
   Start_Time := Calendar.Clock;
   loop
      begin
         Receive(Message_1, From => Sender_1);
      exception
         when No_More_Senders =>
            Put_Line("--- Exception No_More_Senders was raised");
            exit;
      end;
      if Message_1.Kind /= Application_Message then
         Put_Line("Received " & Message_Kind'Image(Message_1.Kind) &
                  " from " & Image(Sender_1) & (Image(Sender_1)'Length + 1..16 => ' ') &
                  " on node " & Image(Message_1.Node));
      end if;
      Incr_Message_Count(Process => Sender_1);
      case Message_1.Kind is
         when Process_Creation =>
            if Sender_1 = Current_Process then
               Put_Line("!!! Process_Creation from Current_Process");
               raise Test_Error;
            end if;
            Number_Of_Processes := Number_Of_Processes + 1;
         when Process_Termination =>
            if Sender_1 = Current_Process then
               Put_Line("!!! Process_Termination from Current_Process");
               raise Test_Error;
            end if;
            if not Member(Sender_1, Of_Table => Known_Processes) then
               Put_Line("!!! Process_Termination from an unknown process");
               raise Test_Error;
            end if;
         when Process_Failure =>
            if Sender_1 = Current_Process then
               Put_Line("!!! Process_Failure from Current_Process");
               raise Test_Error;
            end if;
            if not Member(Sender_1, Of_Table => Known_Processes) then
               Put_Line("!!! Process_Failure from an unknown process");
               raise Test_Error;
            end if;
            Process_Failure_Gotten := True;
            if Hi.Terminate_On_Process_Failure then
               Stop_Time := Calendar.Clock + 60.0;
               Set_Process_Name(Phase => "F");
            end if;
         when Application_Message =>
            case Message_1.Contents.Kind is
               when Hello =>
                  if Sender_1 /= Creator_Process or
                     Creator_Process = Null_Process or
                     Hello_Gotten
                  then
                     Put_Line("!!! unexpected Hello gotten");
                     raise Test_Error;
                  end if;
                  Hi := Message_1.Contents.Hello_Part;
                  Hello_Gotten := True;
                  if Available_Nodes'Length /= Hi.Number_Of_Available_Nodes or
                     Available_Nodes'First /= 1
                  then
                     Put_Line("!!! Error in Available_Nodes");
                     raise Test_Error;
                  end if;
                  Broadcast(The_Message => (Kind          => Noise_Out,
                                            Time_Sent     => Calendar.Clock,
                                            Noise_Part    => The_Noise,
                                            Serial_Number => 0));
                  Send(The_Message => (Kind          => Noise_Out,
                                       Time_Sent     => Calendar.Clock,
                                       Noise_Part    => The_Noise,
                                       Serial_Number => 0),
                       To          => Current_Process);
               when Noise_Out =>
                  Send((Kind          => Noise_In,
                        Time_Sent     => Message_1.Contents.Time_Sent,
                        Noise_Part    => Message_1.Contents.Noise_Part,
                        Serial_Number => Message_1.Contents.Serial_Number),
                       To => Sender_1);
               when Noise_In =>
                  Average_Time(Process            => Sender_1,
                               Time               => Calendar.Clock - Message_1.Contents.Time_Sent,
                               Noise_Back         => Message_1.Contents.Noise_Part,
                               Serial_Number_Back => Message_1.Contents.Serial_Number);
                  if not Hello_Gotten then
                     Put_Line("!!! Noise_In gotten before Hello");
                     raise Test_Error;
                  end if;
                  if Sender_1 = Current_Process then
                     Out_Delay := 10.0;
                  else
                     Out_Delay :=
                        Duration(Random.Uniform(Random_Numeric_Types.Random_Real(Hi.Noise_Out_Delay / 2),
                                                Random_Numeric_Types.Random_Real(Hi.Noise_Out_Delay)));
                  end if;
                  Search(Key    => Sender_1,
                         Within => Known_Processes).Noise_Out_Sender.Send(To     => Sender_1,
                                                                          After  => Out_Delay,
                                                                          Number =>
                                                                             Message_1.Contents.Serial_Number + 1);
            end case;
      end case;
      if Startup_Done then
         exit when Calendar.Clock > Stop_Time;
      elsif Calendar.Clock > Start_Time + 60.0 then
         Startup_Done := True;
         Put_Line("--- Startup done");
         if not Hello_Gotten then
            Put_Line("!!! Startup done when Hello not gotten");
            raise Test_Error;
         end if;
         if not (Hi.Terminate_On_Process_Failure and Process_Failure_Gotten) then
            if Number_Of_Processes < Hi.Number_Of_Processes_To_Create and
               (Current_Process /= Root_Process or
                Random.Probability(0.5))
            then
               Create_Process(On_Node     => Random_Node,
                              New_Process => Son_1);
               Put_Line("--- Just created " & Image(Son_1));
               Send((Hello, Hi), To => Son_1);
               if Number_Of_Processes + 1 < Hi.Number_Of_Processes_To_Create and
                  Random.Probability(0.5)
               then
                  Create_Process(On_Node     => Random_Node,
                                 New_Process => Son_2);
                  Put_Line("--- Just created " & Image(Son_2));
                  Send((Hello, Hi), To => Son_2);
                  if Number_Of_Processes + 2 < Hi.Number_Of_Processes_To_Create and
                     Random.Probability(0.5)
                  then
                     Create_Process(On_Node     => Random_Node,
                                    New_Process => Son_3);
                     Put_Line("--- Just created " & Image(Son_3));
                     Send((Hello, Hi), To => Son_3);
                  end if;
               end if;
            end if;
            Stop_Time := Hi.Stop_Time +
                         Duration(Random.Uniform(0.0,
                                                 Random_Numeric_Types.Random_Real
                                                    (Hi.Max_Termination_Delay)));
            Set_Process_Name(Phase => "");
         end if;
      end if;
   end loop;
   Put_Line("--- Terminating");
   Set_Process_Name(Phase => "T");
   Stop_Noise_Out_Senders;
   Noise_Out_Senders_Stopped := True;
   Put_Line("--- calling Terminate_Current_Process");
   Terminate_Current_Process;
   Put_Line("Number of processes seen : " & Image(Number_Of_Processes));
   Put_Message_Counts;
   Destroy(Known_Processes);
exception
   when Current_Process_Terminated =>
      if not Noise_Out_Senders_Stopped then
         Stop_Noise_Out_Senders;
         Noise_Out_Senders_Stopped := True;
      end if;
      raise;
   when others =>
      if not Noise_Out_Senders_Stopped then
         Stop_Noise_Out_Senders;
         Noise_Out_Senders_Stopped := True;
      end if;
      Terminate_Current_Process;
      raise;
end Test_Distributor;
