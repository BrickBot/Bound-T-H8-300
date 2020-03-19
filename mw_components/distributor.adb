-- GENERIC PACKAGE FOR DISTRIBUTING ADA PROGRAMS ON A DECNET NETWORK
   -----------------------------------------------------------------

-- Creation : 29-NOV-1989 by Mats Weber.


with Varying_Text;

package body Distributor is
------------------------

   Max_Node_Name_Length    : constant := 6;
   Max_Object_Name_Length  : constant := 12;

   type Node_Descriptor is
      record
         Node_Name   : Varying_Text.Text(Max_Node_Name_Length);
         Object_Name : Varying_Text.Text(Max_Object_Name_Length);
      end record;

   type Network_Message_Kind is (Process_Initialization,
                                 Node_Information,
                                 Process_Creation,
                                 Process_Termination,
                                 Process_Failure,
                                 Process_Death,
                                 Application_Message,
                                 Broadcast_Message);

   type Network_Message (Kind : Network_Message_Kind := Application_Message) is
      record
         Originator : Process_Id;
         case Kind is
            when Process_Initialization =>
               New_Process          : Process_Id;
               New_Node,
               Father_Node          : Node_Id;
               Number_Of_Nodes      : Natural;    -- number of Node_Information messages that will follow
            when Node_Information =>
               Node                 : Node_Id;
               Descriptor           : Node_Descriptor;
            when Process_Creation | Process_Termination | Process_Failure =>
               Origin_Node          : Node_Id;
            when Process_Death =>
               null;
            when Application_Message =>
               Destination          : Process_Id;
               Application_Contents : Application_Message_Type;
            when Broadcast_Message =>
               Broadcast_Contents   : Application_Message_Type;
         end case;
      end record;


   task Network_Handler is
   --------------------

      entry Start_Application (Nodes_File_Name : in String);

      entry Create_Process (On_Node     : in Node_Id;
                            New_Process : out Process_Id);

      entry Terminate_Current_Process;

      entry Termination_Complete;


      entry Send (The_Message : in Application_Message_Type;
                  To          : in Process_Id);

      entry Broadcast (The_Message : in Application_Message_Type);

      entry Receive (The_Message : out Message;
                     From        : out Process_Id);


      entry Get_Current_Process (Process : out Process_Id);

      entry Get_Creator_Process (Process : out Process_Id);

      entry Get_Current_Node (Node : out Node_Id);

      entry Get_Last_Available_Node (Last : out Natural);

      entry Get_Available_Nodes (Nodes : out Node_Id_List);


      entry Deliver_Link_Message (The_Message : in Network_Message;
                                  Index       : in Son_Index);

      entry Signal_Link_Termination (Index : in Son_Index);

   end Network_Handler;

   task body Network_Handler is separate;


   function "<" (Left, Right : Process_Id) return Boolean is
   begin
      if Left.Dewey.Depth = Right.Dewey.Depth then
         return Left.Dewey.Path < Right.Dewey.Path;
      else
         return Left.Dewey.Depth < Right.Dewey.Depth;
      end if;
   end "<";

   function "<" (Left, Right : Node_Id) return Boolean is
   begin
      return Right > Left;
   end "<";


   procedure Start_Application (Nodes_File_Name : in String) is
   begin
      Network_Handler.Start_Application(Nodes_File_Name);
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Start_Application;


   procedure Create_Process (On_Node     : in Node_Id;
                             New_Process : out Process_Id) is
   begin
      Network_Handler.Create_Process(On_Node, New_Process);
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Create_Process;


   procedure Terminate_Current_Process is
   begin
      Network_Handler.Terminate_Current_Process;
      Network_Handler.Termination_Complete;
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Terminate_Current_Process;


   procedure Send (The_Message : in Application_Message_Type;
                   To          : in Process_Id) is
   begin
      Network_Handler.Send(The_Message, To);
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Send;


   procedure Broadcast (The_Message : in Application_Message_Type) is
   begin
      Network_Handler.Broadcast(The_Message);
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Broadcast;


   procedure Receive (The_Message : out Message;
                      From        : out Process_Id) is
   begin
      Network_Handler.Receive(The_Message, From);
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Receive;


   function Current_Process return Process_Id is

      Result : Process_Id;

   begin
      Network_Handler.Get_Current_Process(Result);
      return Result;
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Current_Process;


   function Creator_Process return Process_Id is

      Result : Process_Id;

   begin
      Network_Handler.Get_Creator_Process(Result);
      return Result;
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Creator_Process;


   function Current_Node return Node_Id is

      Result : Node_Id;

   begin
      Network_Handler.Get_Current_Node(Result);
      return Result;
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Current_Node;


   function Available_Nodes return Node_Id_List is

      Result_Last : Natural;

   begin
      Network_Handler.Get_Last_Available_Node(Result_Last);
      declare

         Result : Node_Id_List(1..Result_Last);

      begin
         Network_Handler.Get_Available_Nodes(Nodes => Result);
         return Result;
      end;
   exception
      when Tasking_Error =>
         raise Current_Process_Terminated;
   end Available_Nodes;

end Distributor;
