with Buffers,
     Tasking_Services,
     System,
     Storage_Units;

separate (Distributor.Network_Handler)

task body Link_Controller is
-------------------------

   My_Index        : Son_Index;
   Channel         : Starlet.Channel_Type;

   Max_Retries     : constant := 3;              -- Maximum number of times a process creation operation
                                                 -- is retried if it fails because of a timeout.
   Retry_Interval  : constant Duration := 20.0;  -- Interval between retries.


   package Network_Message_Buffers is new Buffers(Network_Message,
                                                  Count => Natural);


   task Reader is
      entry Start;
      entry Start (Device_Name : in String);
      entry Ready;  -- will raise System.Non_Ada_Error if the link could not be opened
   end;

   Incoming_Messages : Network_Message_Buffers.Buffer;

   task Receiver;

   Outgoing_Messages : Network_Message_Buffers.Buffer;

   task Sender;


   function "/" (Left, Right : System.Unsigned_Longword) return System.Unsigned_Longword
      renames System."/";


   procedure Deassign (Channel : in Starlet.Channel_Type) is

      Return_Status : Condition_Handling.Cond_Value_Type;

   begin
      Starlet.Dassgn(Status => Return_Status,
                     Chan   => Channel);
      Check_Condition(Return_Status);
   end Deassign;


   procedure Empty (The_Buffer : in out Network_Message_Buffers.Buffer) is

      Trash : Network_Message;

   begin
      loop
         The_Buffer.Read(Trash);
      end loop;
   exception
      when Tasking_Error => null;             -- because The_Buffer was Killed
   end Empty;


   task body Reader is

      The_Message : Network_Message;

      procedure QIO_Receive (Channel     : in Starlet.Channel_Type;
                             The_Message : out Network_Message) is

         Return_Status    : Condition_Handling.Cond_Value_Type;
         IO_Status_Block  : Starlet.IOSB_Type;

      begin
         if The_Message'Constrained then
            raise Fatal_Error;
         end if;
         Tasking_Services.Task_QIOW(Status => Return_Status,
                                    Chan   => Channel,
                                    Func   => Starlet.IO_ReadVBlk,
                                    IOSB   => IO_Status_Block,
                                    P1     => System.To_Unsigned_Longword
                                                 (The_Message'Address),
                                    P2     => Network_Message'Size /
                                              Storage_Units.Byte_Bits);
         Check_Condition(Return_Status);
         Check_Condition(IO_Status_Block.Status);
      end QIO_Receive;

      pragma Inline(QIO_Receive);

   begin
      declare

         Max_Device_Name_Length  : constant := 250;

         The_Device_Name         : String(1..Max_Device_Name_Length);
         The_Device_Name_Length  : Natural range 0..The_Device_Name'Length;

         Return_Status           : Condition_Handling.Cond_Value_Type;

      begin
         select
            accept Start;
         or
            accept Start (Device_Name : in String) do
               The_Device_Name(1..Device_Name'Length) := Device_Name;
               The_Device_Name_Length := Device_Name'Length;
            end Start;
            for N in 0..Max_Retries loop
               Starlet.Assign(Status => Return_Status,
                              DevNam => The_Device_Name(1..The_Device_Name_Length),
                              Chan   => Channel);
               exit when Condition_Handling.Success(Return_Status) or
                         Condition_Handling.Match_Cond(Return_Status, Starlet.SS_LinkExit) = 0;
               delay Retry_Interval;
            end loop;
            Check_Condition(Return_Status);
         end select;
      exception
         when System.Non_Ada_Error =>               -- because Assign failed
            begin
               Link_Controller.Close;
            exception
               when Tasking_Error => null;          -- because Link_Controller.Close has
            end;                                    -- already been called
            accept Ready do
               raise;                               -- raise the same exception in Sender
            end;
      end;
      begin
         Link_Controller.Reader_Ready;
      exception
         when Tasking_Error =>                      -- Link_Controller.Close was called
            Deassign(Channel);                      -- while the channel was being assigned
            raise;
      end;
      accept Ready;
      loop
         begin
            QIO_Receive(Channel     => Channel,
                        The_Message => The_Message);
         exception
            when System.Non_Ada_Error =>            -- because QIO_Receive failed
               begin
                  Link_Controller.Close;
                  exit;
               exception
                  when Tasking_Error => null;       -- because Link_Controller.Close has
               end;                                 -- already been called
               raise;
         end;
         begin
            Incoming_Messages.Write(The_Message);
         exception
            when Tasking_Error =>                   -- because Incoming_Messages was Killed
               exit;
         end;
         exit when The_Message.Kind = Process_Death;
      end loop;
   end Reader;


   task body Receiver is

      The_Message : Network_Message;

   begin
      loop
         begin
            Incoming_Messages.Read(The_Message);
         exception
            when Tasking_Error =>                   -- because Incoming_Messages was Killed
               exit;
         end;
         Network_Handler.Deliver_Link_Message(The_Message => The_Message,
                                              Index       => My_Index);
         exit when The_Message.Kind = Process_Death;
      end loop;
      Network_Handler.Signal_Link_Termination(Index => My_Index);
   exception
      when Fatal_Error =>                           -- raised in Network_Handler
         Empty(Incoming_Messages);
   end Receiver;


   task body Sender is

      The_Message : Network_Message;

      procedure QIO_Send (Channel     : in Starlet.Channel_Type;
                          The_Message : in Network_Message) is

         Return_Status    : Condition_Handling.Cond_Value_Type;
         IO_Status_Block  : Starlet.IOSB_Type;

      begin
         Tasking_Services.Task_QIOW(Status => Return_Status,
                                    Chan   => Channel,
                                    Func   => Starlet.IO_WriteVBlk,
                                    IOSB   => IO_Status_Block,
                                    P1     => System.To_Unsigned_Longword
                                                 (The_Message'Address),
                                    P2     => The_Message'Size /
                                              Storage_Units.Byte_Bits);
         Check_Condition(Return_Status);
         Check_Condition(IO_Status_Block.Status);
      end QIO_Send;

      pragma Inline(QIO_Send);

   begin
      Reader.Ready;                                    -- raises System.Non_Ada_Error if
      loop                                             -- Reader could not open the link
         begin
            Outgoing_Messages.Read(The_Message);
         exception
            when Tasking_Error =>                      -- because Outgoing_Messages has been Killed
               exit;
         end;
         QIO_Send(Channel     => Channel,
                  The_Message => The_Message);
         exit when The_Message.Kind = Process_Death;
      end loop;
   exception
      when System.Non_Ada_Error =>                     -- because QIO_Send failed or
         begin                                         -- Reader could not open the link
            Link_Controller.Close;
            Empty(Outgoing_Messages);
            raise;
         exception
            when Tasking_Error =>                      -- because Link_Controller.Close
               Empty(Outgoing_Messages);               -- has already been called
         end;
   end Sender;


   task Terminator is
      entry Start;
   end;

   task body Terminator is
   begin
      accept Start;
      Incoming_Messages.Kill;
      Outgoing_Messages.Kill;
   end Terminator;


begin
   select
      accept Open (Index       : in Positive_Son_Index;
                   Device_Name : in String)
      do
         My_Index := Index;
         Reader.Start(Device_Name);
      end Open;
   or
      accept Open (Index       : in Son_Index;
                   The_Channel : in Starlet.Channel_Type)
      do
         My_Index := Index;
         Channel := The_Channel;
         Reader.Start;
      end Open;
   end select;
   --
   Incoming_Messages.Set_Size(To => Network_Message_Buffers.Unbounded);
   Outgoing_Messages.Set_Size(To => Network_Message_Buffers.Unbounded);
   --
   declare

      Channel_Assigned : Boolean := False;

   begin
      loop
         select
            when not Channel_Assigned =>
               accept Reader_Ready;
               Channel_Assigned := True;
         or
            accept Send (The_Message : in Network_Message) do
               Outgoing_Messages.Write(The_Message);
            end;
         or
            accept Close;
            exit;
         end select;
      end loop;
      --
      Terminator.Start;
      if Channel_Assigned then
         Deassign(Channel);
      end if;
   end;
end Link_Controller;
