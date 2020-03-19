with Queues,
     Tables,
     Starlet,
     Condition_Handling,
     Unchecked_Deallocation;

separate (Distributor)

task body Network_Handler is
-------------------------

   Fatal_Error : exception;
      -- Raised when unrecoverable errors or inconsistencies arise.


   task type Link_Controller is
   -------------------------

      entry Open (Index       : in Positive_Son_Index;
                  Device_Name : in String);

      entry Open (Index       : in Son_Index;
                  The_Channel : in Starlet.Channel_Type);

      entry Reader_Ready;    -- used by Reader when the channel has been assigned

      entry Send (The_Message : in Network_Message);

      entry Close;

   end Link_Controller;

   type Link_Controller_Access is access Link_Controller;
      -- This declaration must not be in a nested block
      -- because Network_Handler must be the direct master
      -- of all Link_Controllers.
      -- Otherwise a fatal exception would not be able to
      -- complete the execution of Network_Handler as it
      -- would wait for completion of all Link_Controllers
      -- before propagating the exception outside the nested
      -- block.


   procedure Check_Condition (Status : in Condition_Handling.Cond_Value_Type) is
   begin
      if not Condition_Handling.Success(Status) then
         Condition_Handling.Signal(Status);
      end if;
   end Check_Condition;

   procedure Check_Condition (Status : in Condition_Handling.Word_Cond_Value_Type) is
   begin
      if not Condition_Handling.Success(Status) then
         Condition_Handling.Signal(Condition_Handling.Cond_Value_Type(Status));
      end if;
   end Check_Condition;

   pragma Inline(Check_Condition);


   task body Link_Controller is separate;


   package Node_Tables is new Tables(Key_Type  => Node_Id,
                                     Item_Type => Node_Descriptor,
                                     Count     => Natural);

   procedure Read_Nodes_File (File_Name : in String;
                              Into      : in out Node_Tables.Table) is separate;


begin
   declare

      type Message_With_Originator is
         record
            Originator : Process_Id;
            Contents   : Message;
         end record;

      package Message_Queues is new Queues(Item_Type => Message_With_Originator,
                                           Count     => Natural);

      package Process_Tables is new Tables(Key_Type  => Process_Id,
                                           Item_Type => Node_Id,
                                           Count     => Natural);

      Father_Process,
      Current_Process,
      New_Son_Process         : Process_Id;

      Father_Node,
      Current_Node,
      New_Son_Node            : Node_Id;

      Application_Messages    : Message_Queues.Queue;
      Active_Processes        : Process_Tables.Table;
      Available_Nodes         : Node_Tables.Table;

      Father_Index            : constant Son_Index := 0;
      Links                   : array (Son_Index) of Link_Controller_Access;
      Father_Link             : Link_Controller_Access renames Links(Father_Index);
      Son_Counter             : Son_Index := Father_Index;
      Terminating_Links       : array (Positive_Son_Index) of Boolean := (others => False);
                                -- Links for which Network_Handler is awaiting Signal_Link_Termination.

      The_Network_Message     : Network_Message;
      Arrival_Link_Index      : Son_Index;

      Terminating             : Boolean := False;
      Started                 : Boolean;

      subtype Application_Network_Message is Network_Message(Kind => Application_Message);


      procedure Dispose is new Unchecked_Deallocation(Link_Controller,
                                                      Link_Controller_Access);


      function Next_Link_On_Path (From : in Process_Id := Current_Process;
                                  To   : in Process_Id)
         return Son_Index is
      begin
         if To.Dewey.Depth > From.Dewey.Depth and then
            To.Dewey.Path(1..From.Dewey.Depth) = From.Dewey.Path
         then
            return To.Dewey.Path(From.Dewey.Depth + 1);
         else
            return Father_Index;
         end if;
      end Next_Link_On_Path;

      pragma Inline(Next_Link_On_Path);


      procedure Send_Everywhere (The_Message : in Network_Message) is
         -- Sends The_Message to all links
      begin
         for I in Father_Index..Son_Counter loop
            if Links(I) /= null then
               begin
                  Links(I).Send(The_Message);
               exception
                  when Tasking_Error => null;
               end;
            end if;
         end loop;
      end Send_Everywhere;

      procedure Send_Everywhere (The_Message : in Network_Message;
                                 Except_To   : in Son_Index) is
         -- Sends The_Message to all links except Except_To
      begin
         for I in Father_Index..Son_Counter loop
            if I /= Except_To and Links(I) /= null then
               begin
                  Links(I).Send(The_Message);
               exception
                  when Tasking_Error => null;
               end;
            end if;
         end loop;
      end Send_Everywhere;

      pragma Inline(Send_Everywhere);


      procedure Deliver_To_Application (The_Message : in Message;
                                        From        : in Process_Id) is
      begin
         if not Terminating then
            Message_Queues.Put(Item => (Originator => From,
                                        Contents   => The_Message),
                               Into => Application_Messages);
         end if;
      end Deliver_To_Application;

      pragma Inline(Deliver_To_Application);


      procedure Route_Message (The_Message : in Application_Network_Message) is
      begin
         if The_Message.Destination = Current_Process then
            Deliver_To_Application(The_Message => (Kind     => Application_Message,
                                                   Contents => The_Message.Application_Contents),
                                   From        => The_Message.Originator);
         else
            declare

               Route_Link : Link_Controller_Access renames
                            Links(Next_Link_On_Path(To => The_Message.Destination));

            begin
               if Route_Link /= null then
                  begin
                     Route_Link.Send(The_Message);
                  exception
                     when Tasking_Error => null;
                  end;
               end if;
            end;
         end if;
      end Route_Message;

      pragma Inline(Route_Message);


      function No_More_Sons_Active return Boolean is
      begin
         return Links(1..Son_Counter) = (1..Son_Counter => null) and
                Terminating_Links(1..Son_Counter) = (1..Son_Counter => False);
      end No_More_Sons_Active;

      pragma Inline(No_More_Sons_Active);


      procedure Initialize_Son is

         New_Son_Node_Descriptor : constant Node_Descriptor :=
                                   Node_Tables.Search(Key    => New_Son_Node,
                                                      Within => Available_Nodes);


         procedure Send_Node_Information (Node       : in Node_Id;
                                          Descriptor : in Node_Descriptor) is
         begin
            Links(Son_Counter).Send(The_Message => (Kind       => Node_Information,
                                                    Originator => Current_Process,
                                                    Node       => Node,
                                                    Descriptor => Descriptor));
         end Send_Node_Information;

         pragma Inline(Send_Node_Information);

         procedure Send_All_Node_Informations is
            new Node_Tables.Traversal(Action => Send_Node_Information);


         procedure Send_Process_Creation (Process : in Process_Id;
                                          Node    : in Node_Id) is
         begin
            Links(Son_Counter).Send(The_Message => (Kind        => Process_Creation,
                                                    Originator  => Process,
                                                    Origin_Node => Node));
         end Send_Process_Creation;

         pragma Inline(Send_Process_Creation);

         procedure Send_All_Process_Creations is
            new Process_Tables.Traversal(Action => Send_Process_Creation);

      begin
         -- create remote process
         Links(Son_Counter) := new Link_Controller;
         Links(Son_Counter).Open(Index       => Son_Counter,
                                 Device_Name => Varying_Text.To_String(New_Son_Node_Descriptor.Node_Name) &
                                                "::""0=" &
                                                Varying_Text.To_String(New_Son_Node_Descriptor.Object_Name) &
                                                '"');
         -- broadcast Process_Creation
         Deliver_To_Application(The_Message => (Kind => Process_Creation,
                                                Node => New_Son_Node),
                                From        => New_Son_Process);
         Send_Everywhere(The_Message => (Kind        => Process_Creation,
                                         Originator  => New_Son_Process,
                                         Origin_Node => New_Son_Node),
                         Except_To   => Son_Counter);
         begin
            -- send initialization message
            Links(Son_Counter).Send(The_Message => (Kind            => Process_Initialization,
                                                    Originator      => Current_Process,
                                                    New_Process     => New_Son_Process,
                                                    New_Node        => New_Son_Node,
                                                    Father_Node     => Current_Node,
                                                    Number_Of_Nodes => Node_Tables.Card(Available_Nodes)));
            -- send node information messages for all available nodes
            -- to the new son
            Send_All_Node_Informations(Available_Nodes);
            -- send Process_Creation messages for all active processes
            -- to the new son
            Send_All_Process_Creations(Active_Processes);
         exception
            when Tasking_Error => null;
         end;
         -- add the new son to the set of active processes
         Process_Tables.Insert(Key  => New_Son_Process,
                               Item => New_Son_Node,
                               Into => Active_Processes);
      end Initialize_Son;

      pragma Inline(Initialize_Son);


      procedure Handle_Unexpected_Link_Termination (Index : in Son_Index) is

         Failed_Processes : Process_Tables.Table;


         procedure Insert_Into_Failed_Processes (Process : in Process_Id;
                                                 Node    : in Node_Id) is
         begin
            if Next_Link_On_Path(To => Process) = Index then
               Process_Tables.Insert(Key  => Process,
                                     Item => Node,
                                     Into => Failed_Processes);
            end if;
         end Insert_Into_Failed_Processes;

         pragma Inline(Insert_Into_Failed_Processes);

         procedure Insert_All_Into_Failed_Processes is
            new Process_Tables.Traversal(Action => Insert_Into_Failed_Processes);


         procedure Send_Process_Failure (Process : in Process_Id;
                                         Node    : in Node_Id) is
         begin
            Deliver_To_Application(The_Message => (Kind => Process_Failure,
                                                   Node => Node),
                                   From        => Process);
            Send_Everywhere(The_Message => (Kind        => Process_Failure,
                                            Originator  => Process,
                                            Origin_Node => Node),
                            Except_To   => Index);
            Process_Tables.Remove(Key => Process, From => Active_Processes);
         end Send_Process_Failure;

         pragma Inline(Send_Process_Failure);

         procedure Send_All_Process_Failures is
            new Process_Tables.Traversal(Action => Send_Process_Failure);

      begin
         Dispose(Links(Index));
         Insert_All_Into_Failed_Processes(Active_Processes);
         Send_All_Process_Failures(Failed_Processes);
         Process_Tables.Destroy(Failed_Processes);
      end Handle_Unexpected_Link_Termination;

      pragma Inline(Handle_Unexpected_Link_Termination);


   begin
      declare

         Father_Channel   : Starlet.Channel_Type;
         Return_Status    : Condition_Handling.Cond_Value_Type;

         Number_Of_Nodes  : Natural;

      begin
         -- try to open a link with the father process
         Starlet.Assign(Status => Return_Status,
                        DevNam => "SYS$NET",
                        Chan   => Father_Channel);
         -- if Starlet.SS_NoSuchDev is returned, then the current process
         -- is the root of the tree.
         if Condition_Handling.Match_Cond(Return_Status, Starlet.SS_NoSuchDev) = 0 then
            Check_Condition(Return_Status);
            -- create father link controller
            Father_Link := new Link_Controller;
            Father_Link.Open(Index       => Father_Index,
                             The_Channel => Father_Channel);
            -- receive initialization message from the father
            select
               accept Deliver_Link_Message (The_Message : in Network_Message;
                                            Index       : in Son_Index)
               do
                  if Index /= Father_Index or The_Message.Kind /= Process_Initialization then
                     raise Fatal_Error;
                  end if;
                  Current_Process := The_Message.New_Process;
                  Father_Process  := The_Message.Originator;
                  Current_Node    := The_Message.New_Node;
                  Father_Node     := The_Message.Father_Node;
                  Number_Of_Nodes := The_Message.Number_Of_Nodes;
               end Deliver_Link_Message;
            or
               accept Signal_Link_Termination (Index : in Son_Index);
               raise Fatal_Error;
            end select;
            -- receive node information messages from the father
            for I in 1..Number_Of_Nodes loop
               select
                  accept Deliver_Link_Message (The_Message : in Network_Message;
                                               Index       : in Son_Index)
                  do
                     if Index /= Father_Index or
                        The_Message.Kind /= Node_Information or
                        The_Message.Originator /= Father_Process
                     then
                        raise Fatal_Error;
                     end if;
                     Node_Tables.Insert(Key  => The_Message.Node,
                                        Item => The_Message.Descriptor,
                                        Into => Available_Nodes);
                  end Deliver_Link_Message;
               or
                  accept Signal_Link_Termination (Index : in Son_Index);
                  raise Fatal_Error;
               end select;
            end loop;
            Process_Tables.Insert(Key  => Father_Process,
                                  Item => Father_Node,
                                  Into => Active_Processes);
            Deliver_To_Application(The_Message => (Kind => Process_Creation,
                                                   Node => Father_Node),
                                   From        => Father_Process);
            Started := True;
         else
            Current_Process := Root_Process;
            Father_Process  := Null_Process;
            Current_Node := Root_Node;
            Father_Node  := Null_Node;
            Started := False;
         end if;
      end;
      --
      loop
         begin
            select
               when not Terminating and not Started =>
                  accept Start_Application (Nodes_File_Name : in String) do
                     Read_Nodes_File(File_Name => Nodes_File_Name,
                                     Into      => Available_Nodes);
                  end;
                  Started := True;
            or
               when not Terminating and Started =>
                  accept Start_Application (Nodes_File_Name : in String) do
                     raise Application_Already_Started;
                  end;
            or
               when not Terminating and Started =>
                  accept Create_Process (On_Node     : in Node_Id;
                                         New_Process : out Process_Id)
                  do
                     if On_Node = Null_Node then
                        raise Node_Is_Null;
                     elsif On_Node = Root_Node then
                        raise Node_Is_Root;
                     end if;
                     New_Son_Node := On_Node;
                     Son_Counter := Son_Counter + 1;
                     New_Son_Process := (Dewey =>
                                            (Depth => Current_Process.Dewey.Depth + 1,
                                             Path  => Current_Process.Dewey.Path & Son_Counter));
                     New_Process := New_Son_Process;
                  end Create_Process;
                  Initialize_Son;
            or
               when not Terminating =>
                  accept Terminate_Current_Process;
                  Terminating := True;
                  Send_Everywhere((Kind        => Process_Termination,
                                   Origin_Node => Current_Node,
                                   Originator  => Current_Process));
                  exit when No_More_Sons_Active;
            or
               when not Terminating and Started =>
                  accept Send (The_Message : in Application_Message_Type;
                               To          : in Process_Id)
                  do
                     if To = Null_Process then
                        raise Process_Is_Null;
                     end if;
                     The_Network_Message := (Kind                 => Application_Message,
                                             Originator           => Current_Process,
                                             Destination          => To,
                                             Application_Contents => The_Message);
                  end Send;
                  Route_Message(The_Network_Message);
            or
               when not Terminating and Started =>
                  accept Broadcast (The_Message : in Application_Message_Type) do
                     The_Network_Message := (Kind               => Broadcast_Message,
                                             Originator         => Current_Process,
                                             Broadcast_Contents => The_Message);
                  end Broadcast;
                  Send_Everywhere(The_Network_Message);
            or
               when not Terminating and Started and
                    not Message_Queues.Empty(Application_Messages) =>
                  accept Receive (The_Message : out Message;
                                  From        : out Process_Id)
                  do
                     declare

                        Next_Message : Message_With_Originator;

                     begin
                        Message_Queues.Get(From => Application_Messages,
                                           Item => Next_Message);
                        The_Message := Next_Message.Contents;
                        From        := Next_Message.Originator;
                     end;
                  end Receive;
            or
               when not Terminating and Started and
                    Message_Queues.Empty(Application_Messages) and
                    Process_Tables.Empty(Active_Processes) =>
                  accept Receive (The_Message : out Message;
                                  From        : out Process_Id)
                  do
                     raise No_More_Senders;
                  end Receive;
            or
               when not Terminating =>
                  accept Get_Current_Process (Process : out Process_Id) do
                     Process := Current_Process;
                  end;
            or
               when not Terminating =>
                  accept Get_Creator_Process (Process : out Process_Id) do
                     Process := Father_Process;
                  end;
            or
               when not Terminating =>
                  accept Get_Current_Node (Node : out Node_Id) do
                     Node := Current_Node;
                  end;
            or
               when not Terminating and Started =>
                  accept Get_Last_Available_Node (Last : out Natural) do
                     Last := Node_Tables.Card(Available_Nodes);
                  end;
                  accept Get_Available_Nodes (Nodes : out Node_Id_List) do
                     declare

                        Index : Natural range 0..Nodes'Last := 0;

                        procedure Put_Into_Nodes (Node       : in Node_Id;
                                                  Descriptor : in Node_Descriptor) is
                        begin
                           Index := Index + 1;
                           Nodes(Index) := Node;
                        end Put_Into_Nodes;

                        pragma Inline(Put_Into_Nodes);

                        procedure Put_All_Into_Nodes is
                           new Node_Tables.Traversal(Action => Put_Into_Nodes);

                     begin
                        Put_All_Into_Nodes(Available_Nodes);
                        if Index /= Nodes'Last then
                           raise Fatal_Error;
                        end if;
                     end;
                  end Get_Available_Nodes;
            or
               accept Deliver_Link_Message (The_Message : in Network_Message;
                                            Index       : in Son_Index)
               do
                  The_Network_Message := The_Message;
                  Arrival_Link_Index  := Index;
               end Deliver_Link_Message;
               case The_Network_Message.Kind is
                  when Process_Initialization | Node_Information =>
                     raise Fatal_Error;
                  when Process_Creation =>
                     Deliver_To_Application(The_Message => (Kind => Process_Creation,
                                                            Node => The_Network_Message.Origin_Node),
                                            From        => The_Network_Message.Originator);
                     Send_Everywhere(The_Network_Message, Except_To => Arrival_Link_Index);
                     Process_Tables.Insert(Key  => The_Network_Message.Originator,
                                           Item => The_Network_Message.Origin_Node,
                                           Into => Active_Processes);
                  when Process_Termination =>
                     Deliver_To_Application(The_Message => (Kind => Process_Termination,
                                                            Node => The_Network_Message.Origin_Node),
                                            From        => The_Network_Message.Originator);
                     Send_Everywhere(The_Network_Message, Except_To => Arrival_Link_Index);
                     Process_Tables.Remove(Key  => The_Network_Message.Originator,
                                           From => Active_Processes);
                  when Process_Failure =>
                     Deliver_To_Application(The_Message => (Kind => Process_Failure,
                                                            Node => The_Network_Message.Origin_Node),
                                            From        => The_Network_Message.Originator);
                     Send_Everywhere(The_Network_Message, Except_To => Arrival_Link_Index);
                     begin
                        Process_Tables.Remove(Key  => The_Network_Message.Originator,
                                              From => Active_Processes);
                     exception
                        when Process_Tables.Nonexistent_Key => null;
                     end;
                  when Process_Death =>
                     if Arrival_Link_Index = Father_Index or else Terminating_Links(Arrival_Link_Index) then
                        raise Fatal_Error;
                     end if;
                     begin
                        Links(Arrival_Link_Index).Close;
                     exception
                        when Tasking_Error => null;
                     end;
                     Dispose(Links(Arrival_Link_Index));
                     Terminating_Links(Arrival_Link_Index) := True;
                  when Application_Message =>
                     Route_Message(The_Network_Message);
                  when Broadcast_Message =>
                     Deliver_To_Application(The_Message => (Kind     => Application_Message,
                                                            Contents => The_Network_Message.Broadcast_Contents),
                                            From        => The_Network_Message.Originator);
                     Send_Everywhere(The_Network_Message, Except_To => Arrival_Link_Index);
               end case;
            or
               accept Signal_Link_Termination (Index : in Son_Index) do
                  Arrival_Link_Index := Index;
               end;
               if Arrival_Link_Index = Father_Index or else not Terminating_Links(Arrival_Link_Index) then
                  -- unexpected termination of a link,
                  -- send Process_Failure messages for unreachable processes
                  -- and update state information
                  Handle_Unexpected_Link_Termination(Index => Arrival_Link_Index);
               else
                  Terminating_Links(Arrival_Link_Index) := False;
               end if;
               exit when Terminating and No_More_Sons_Active;
            end select;
         exception
            when No_More_Senders |
                 Node_Is_Root |
                 Process_Is_Null |
                 Node_Is_Null |
                 Application_Already_Started => null;
         end;
      end loop;
      --
      if Father_Link /= null then
         Father_Link.Send(The_Message => (Kind       => Process_Death,
                                          Originator => Current_Process));
         loop
            select
               accept Deliver_Link_Message (The_Message : in Network_Message;
                                            Index       : in Son_Index)
               do
                  if Index /= Father_Index or
                     The_Message.Kind = Process_Initialization or
                     The_Message.Kind = Node_Information or
                     The_Message.Kind = Process_Death
                  then
                     raise Fatal_Error;
                  end if;
               end Deliver_Link_Message;
            or
               accept Signal_Link_Termination (Index : in Son_Index) do
                  Arrival_Link_Index := Index;
               end;
               if Arrival_Link_Index /= Father_Index then
                  raise Fatal_Error;
               end if;
               exit;
            end select;
         end loop;
         Dispose(Father_Link);
      end if;
      Message_Queues.Destroy(Application_Messages);
      Process_Tables.Destroy(Active_Processes);
   exception
      when others =>
         -- make sure all Link_Controllers are Closed
         for I in Father_Index..Son_Counter loop
            if Links(I) /= null then
               begin
                  Links(I).Close;
               exception
                  when Tasking_Error =>    -- because Links(I).all is completed
                     abort Links(I).all;
                     Dispose(Links(I));
               end;
            end if;
         end loop;
         -- make sure all Link_Controllers are completed
         for I in Father_Index..Son_Counter loop
            if Links(I) /= null then
               begin
                  Links(I).Close;
               exception
                  when Tasking_Error =>    -- because Links(I).all is completed
                     abort Links(I).all;
                     Dispose(Links(I));
               end;
            end if;
         end loop;
         Message_Queues.Destroy(Application_Messages);
         Process_Tables.Destroy(Active_Processes);
         raise;
   end;
   accept Termination_Complete;
end Network_Handler;
