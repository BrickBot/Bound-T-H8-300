-- GENERIC PACKAGE FOR DISTRIBUTING ADA PROGRAMS ON A DECNET NETWORK
   -----------------------------------------------------------------

-- Revision :  7-FEB-1990 by Mats Weber, added constants Null_Process and
--                                       Null_Node.
-- Revision : 11-JAN-1990 by Mats Weber, added Process_Failure to enumeration
--                                       type Message_Kind.
-- Revision : 15-DEC-1989 by Mats Weber, added private type Node_Id.

-- Creation : 29-NOV-1989 by Mats Weber.


generic
   type Application_Message_Type is private;
      -- Type of the messages that can be sent
      -- between processes. Must not contain
      -- subcomponents of an access type.
package Distributor is
-------------------

   type Process_Id is private;
      -- Each value of this type identifies a process.
      -- Its default initial value is Null_Process.

   Null_Process : constant Process_Id;
      -- Id of a nonexistent process.

   Root_Process : constant Process_Id;
      -- Id of the root process
      -- (process that started the distributed application).

   function "<" (Left, Right : Process_Id) return Boolean;
      -- Total ordering on Process_Id, useful for creating tables.
      -- The relation Null_Process < Root_Process < <any other process> holds.


   type Node_Id is private;
      -- Each value of this type identifies a physical node (processor)
      -- on which processes can execute.
      -- Node_Id does not have a default initial value.

   Null_Node : constant Node_Id;
      -- Id of a nonexistent node.

   Root_Node : constant Node_Id;
      -- Node_Id of the root process.

   type Node_Id_List is array (Positive range <>) of Node_Id;

   function "<" (Left, Right : Node_Id) return Boolean;
      -- Total ordering on Node_Id, useful for creating tables.
      -- The relation Null_Node < Root_Node < <any other node> holds.


   type Message_Kind is (Process_Creation,
                         Process_Termination,
                         Process_Failure,
                         Application_Message);

   type Message (Kind : Message_Kind := Application_Message) is
      record
         case Kind is
            when Process_Creation | Process_Termination | Process_Failure =>
               Node     : Node_Id;
                  -- node on which the process
                  -- was started or terminated or failed.
            when Application_Message =>
               Contents : Application_Message_Type;
                  -- contents of the message given to Send.
         end case;
      end record;


   -- Application Startup

   procedure Start_Application (Nodes_File_Name : in String);
      -- Starts a distributed application.
      -- This procedure must be called exactly once, and only from
      -- the root process, otherwise Application_Already_Started
      -- will be raised.
      -- Nodes_File_Name must be the name of a text file whose
      -- lines each contain a DECnet node name and a
      -- DECnet object name separated by spaces and/or tabs.
      -- Create_Process uses this information to open links with
      -- remote nodes.
      -- Error_In_Nodes_File or an exception in Text_IO will be raised
      -- if Nodes_File_Name does not contain the right information.
      -- Each value of type Node_Id except Null_Node and Root_Node will
      -- be mapped to a line of this file and the ordering of Node_Id
      -- will reflect the order of the lines in the file.
      -- Any call to Create_Process, Send, Broadcast, Receive or
      -- Available_Nodes in the root process will block until
      -- Start_Application has been called.
      -- Nodes_File_Name will be read only in the root process.
      -- It does not have to exist on other nodes.


   -- Process Creation and Termination Primitives

   procedure Create_Process (On_Node     : in Node_Id;
                             New_Process : out Process_Id);
      -- Creates a new process on node On_Node.
      -- Returns the new process's id in New_Process.
      -- Will raise Node_Is_Null if On_Node = Null_Node.
      -- Will raise Node_Is_Root if On_Node = Root_Node (this is because
      -- Distributor does not have the information needed to
      -- create a process on a node that is not in Nodes_File_Name).
      -- All other processes, including the current process,
      -- will receive a Process_Creation message with
      -- From = New_Process and Node = On_Node.
      -- New processes receive Process_Creation messages
      -- from all already existent processes, but not
      -- from themselves.
      -- Several processes can be created on the same node.

   procedure Terminate_Current_Process;
      -- Terminates the current process, discarding
      -- any pending messages.
      -- All other processes will receive a Process_Termination
      -- message with From = Current_Process.


   -- Communication Primitives

   -- Messages are always received in the order in
   -- which they were sent.

   procedure Send (The_Message : in Application_Message_Type;
                   To          : in Process_Id);
      -- Sends The_Message to To.
      -- No exception will be raised if To terminates
      -- and does not receive the message.
      -- Will raise Process_Is_Null if To = Null_Process.

   procedure Broadcast (The_Message : in Application_Message_Type);
      -- Sends The_Message to all live processes.

   procedure Receive (The_Message : out Message;
                      From        : out Process_Id);
      -- Waits until the next message arrives and delivers
      -- it in The_Message.
      -- The originator of the message is returned in From.
      -- Will raise No_More_Senders if no more messages will
      -- be delivered from any process (that is, if all other processes
      -- have terminated and all their messages including
      -- Process_Termination have been Received).


   -- Queries

   function Current_Process return Process_Id;
      -- Returns the id of the current process.

   function Creator_Process return Process_Id;
      -- Returns the id of the process that created
      -- the current process.
      -- Equal to Null_Process if the current
      -- process is the root process.

   function Current_Node return Node_Id;
      -- Returns the id of the node on which the current
      -- process is executing.

   function Available_Nodes return Node_Id_List;
      -- Returns an ordered list of all available nodes
      -- that were read from from Nodes_File_Name when the
      -- application was started by the root process.
      -- Does not include Root_Node or Null_Node.


   No_More_Senders,
   Node_Is_Root,
   Process_Is_Null,
   Node_Is_Null,
   Application_Already_Started,
   Error_In_Nodes_File          : exception;


   -- Process_Failure messages will be delivered (through Receive)
   -- for all processes that could not be created or have become
   -- unreachable because of a network error or because they have
   -- been stopped by some external means.
   -- Receiving Process_Failure for some process does not necessarily
   -- mean that this process has actually terminated, but simply
   -- that it has become unreachable.

   Current_Process_Terminated   : exception;
      -- Will be raised by all subprograms except "<"
      -- if the current process has been terminated by a call to
      -- Terminate_Current_Process or if the current process
      -- has entered an inconsistent state (because of a network or
      -- internal error, which normally should not happen).
      --
      -- Applications using distributor should always have handlers
      -- like the following that ensure termination:
      --
      -- exception
      --    when Current_Process_Terminated =>
      --       raise;
      --    when others =>
      --       Terminate_Current_Process;
      --       raise;
      -- end Application;

private

   Max_Number_Of_Sons   : constant := 100;
   Max_Tree_Depth       : constant := 50;
   Max_Number_Of_Nodes  : constant := 1000;


   type Son_Index  is range  0..Max_Number_Of_Sons;
   type Path_Index is range -1..Max_Tree_Depth;

   subtype Positive_Son_Index  is Son_Index  range 1..Son_Index'Last;
   subtype Positive_Path_Index is Path_Index range 1..Path_Index'Last;

   type Process_Path is
      array (Positive_Path_Index range <>) of Positive_Son_Index;

   type Variable_Process_Path (Depth : Path_Index := -1) is
      record
         Path : Process_Path(1..Depth);
      end record;

   type Process_Id is
      record
         Dewey : Variable_Process_Path;
      end record;

   Null_Process : constant Process_Id := (Dewey => (Depth => -1,
                                                    Path  => (others => 1)));

   Root_Process : constant Process_Id := (Dewey => (Depth => 0,
                                                    Path  => (others => 1)));


   type Node_Id is range -1..Max_Number_Of_Nodes;

   Null_Node : constant Node_Id := -1;

   Root_Node : constant Node_Id := 0;


   pragma Inline("<");

end Distributor;
