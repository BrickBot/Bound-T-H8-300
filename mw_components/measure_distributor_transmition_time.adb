-- PROGRAM TO MEASURE THE TRANSMITION TIME OF DISTRIBUTOR MESSAGES
   ---------------------------------------------------------------

-- Creation : 22-DEC-1989 by Mats Weber.


with Distributor,
     Calendar,
     Min_Max_Functions,
     Random,
     Number_Images,
     User_Interface,
     Text_IO,
     VMS_System;

use Calendar;

procedure Measure_Distributor_Transmition_Time is
----------------------------------------------

   Inconsistency : exception;


   function Natural_Answer is new User_Interface.Integer_Answer(Natural);


   Noise_Message_Size : constant Natural := Natural_Answer("Size of noise messages in bytes : ");

   type Byte is range -100..100;

   for Byte'Size use 8;

   type Message_Noise is array (1..Noise_Message_Size) of Byte;

   function Uniform_Noise is new Random.Enumeration_Uniform(Byte);


   type Noise_Kind is (Initialization, Timed_Noise, Results);

   type Noise (Kind : Noise_Kind := Initialization) is
      record
         case Kind is
            when Initialization =>
               Chain_Length : Natural;
               N_Messages   : Natural;
            when Timed_Noise =>
               Time_Sent    : Calendar.Time;
               Noise_Part   : Message_Noise;
            when Results =>
               Average      : Float;
               Min, Max     : Duration;
         end case;
      end record;

   package Noise_Distributor is new Distributor(Noise);

   use Noise_Distributor;


   Remaining_Length   : Natural;
   N_Messages         : Natural;

   The_Message        : Message;
   Sender, Son        : Process_Id;
   Start_Time         : Calendar.Time;
   No_Noise_Received  : Boolean := True;


   function Image is new Number_Images.Integer_Image(Natural);
   function Image is new Number_Images.Float_Image(Float, Default_Fore => 2, Default_Aft => 3);
   function Image is new Number_Images.Fixed_Image(Duration, Default_Fore => 2, Default_Aft => 3);

begin
   if Current_Process = Root_Process then
      Start_Application(Nodes_File_Name => "Node_Names.Dat");
      Remaining_Length := Natural_Answer("Chain Length : ");
      N_Messages       := Natural_Answer("Number of messages : ");
   else
      loop
         Receive(The_Message, From => Sender);
         exit when The_Message.Kind = Application_Message;
      end loop;
      Remaining_Length := The_Message.Contents.Chain_Length;
      N_Messages       := The_Message.Contents.N_Messages;
   end if;
   begin
      VMS_System.Set_Process_Name(To => " - " & Image(Remaining_Length) & " - ");
   exception
      when others => null;
   end;
   if Remaining_Length > 0 then
      Start_Time := Calendar.Clock;
      Create_Process(On_Node     => Available_Nodes(1 + Remaining_Length mod Available_Nodes'Length),
                     New_Process => Son);
      Send((Kind         => Initialization,
            Chain_Length => Remaining_Length - 1,
            N_Messages   => N_Messages),
           To => Son);
      if Current_Process = Root_Process then
         loop
            Receive(The_Message, From => Sender);
            if The_Message.Kind = Application_Message then
               case The_Message.Contents.Kind is
                  when Initialization =>
                     raise Inconsistency;
                  when Timed_Noise =>
                     Send(The_Message.Contents, To => Sender);
                     if No_Noise_Received then
                        Text_IO.Put_Line("Sartup Time : " & Image(Calendar.Clock - Start_Time));
                        No_Noise_Received := False;
                     end if;
                  when Results =>
                     exit;
               end case;
            end if;
         end loop;
         Text_IO.Put_Line("Average : " & Image(The_Message.Contents.Average));
         Text_IO.Put_Line("Min     : " & Image(The_Message.Contents.Min));
         Text_IO.Put_Line("Max     : " & Image(The_Message.Contents.Max));
      end if;
   else
      Send((Kind       => Timed_Noise,
            Time_Sent  => Calendar.Clock,
            Noise_Part => (others => Uniform_Noise)),
           To => Root_Process);
      declare

         The_Time    : Duration;
         Max_Time    : Duration := Duration'First;
         Min_Time    : Duration := Duration'Last;
         Total_Time  : Float := 0.0;
         N           : Natural := 0;

         function Min is new Min_Max_Functions.Minimum(Duration);
         function Max is new Min_Max_Functions.Maximum(Duration);

      begin
         while N < N_Messages loop
            Receive(The_Message, From => Sender);
            if The_Message.Kind = Application_Message then
               The_Time := Calendar.Clock - The_Message.Contents.Time_Sent;
               Max_Time := Max(Max_Time, The_Time);
               Min_Time := Min(Min_Time, The_Time);
               Total_Time := Total_Time + Float(The_Time);
               N := N + 1;
               Send((Kind       => Timed_Noise,
                     Time_Sent  => Calendar.Clock,
                     Noise_Part => The_Message.Contents.Noise_Part),
                    To => Root_Process);
            end if;
         end loop;
         Send((Kind    => Results,
               Average => Total_Time / Float(N),
               Min     => Min_Time,
               Max     => Max_Time),
              To => Root_Process);
      end;
   end if;
   Terminate_Current_Process;
exception
   when Current_Process_Terminated =>
      raise;
   when others =>
      Terminate_Current_Process;
      raise;
end Measure_Distributor_Transmition_Time;
