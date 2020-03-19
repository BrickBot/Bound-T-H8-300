-- STATE PROGRAM FOR VAX/VMS
   -------------------------

-- Revision : 22-Oct-1990 by Mats Weber, removed declarations duplicating those in
--                                       Starlet and Lib. (State compiles under
--                                       VAX Ada 2.2).
-- Revision : 23-JUL-1990 by Mats Weber, added qualifier /show with many additional
--                                       fields and made sorting order customizable.
-- Revision : 11-APR-1990 by Mats Weber, use CLI for parsing command line.
-- Revision : 10-APR-1990 by Mats Weber, use Starlet.Process_Scan for obtaining
--                                       information on all processes in the Vax cluster.
-- Revision : 10-SEP-1986 by Mats Weber, use package UTILITIES for function MAX.
-- Revision :  6-SEP-1986 by Mats Weber, use records for strings of variable
--                                       length (package TEXT_TYPE).
-- Revision : 18-AUG-1986 by Mats Weber, use the "returned length" feature of
--                                       $GETJPI to know the length of the
--                                       returned strings.
-- Revision : 31-JUL-1986 by Mats Weber, correctly display process names with
--                        nulls in the middle (the previous version truncated
--                        at the first null character)
-- Revision :  9-JUL-1986 by Mats Weber, do not display the image names of
--                                       suspended processes.
-- Revision : 19-JUN-1986 by Mats Weber, use of QUICK_SORT instead of GENSETS.
-- Revision :  4-DEC-1985 by Mats Weber.
-- Revision : 29-NOV-1985 by Mats Weber.

-- Creation : 11-JAN-1985 by Pierre Breguet.


with System,
     Starlet,
     Condition_Handling,
     Lib,
     CLI,
     Unchecked_Conversion,
     Storage_Units,
     General_Sets,
     Text_IO,
     Character_Handler,
     String_Handler,
     String_Case_Conversions,
     Number_Images,
     Min_Max_Functions;

procedure State is
---------------

   package Text_Type is
   -----------------

      type Text_Index is new System.Unsigned_Word;

      type Text_String is array (Text_Index range <>) of Character;

      type Text (Max_Length : Text_Index) is
         record
            Length : Text_Index := 0;
            Value  : Text_String(1..Max_Length);
         end record;


      function Equal (T1, T2 : Text) return Boolean;
      function "<"   (T1, T2 : Text) return Boolean;

      procedure Assign (T : out Text; S : in String);

      function To_String (T : Text) return String;

      procedure Put (Item : in Text; Width : in Text_IO.Field := 0);


      pragma Inline(Equal, "<", Assign, Put);

   end Text_Type;

   use Text_Type;


   Max_Time_Image_Length : constant := 50;

   package Process_Info_Type is
   -------------------------

      type Process_State is (COLPG, MWAIT, CEF, PFW, LEF, LEFO,
                             HIB, HIBO, SUSP, SUSPO, FPG, COM, COMO, CUR);

      type Process_Mode is (Detached, Network, Batch, Interactive);

      Max_Username_Length      : constant := 12;
      Max_Node_Name_Length     : constant := 6;
      Max_Process_Name_Length  : constant := 15;
      Max_Terminal_Length      : constant := 8;
      Max_Image_Name_Length    : constant := Starlet.Nam_C_MaxRSS;

      type Process_Info is
         record
            Username          : Text(Max_Username_Length);
            Node_Name         : Text(Max_Node_Name_Length);
            Name              : Text(Max_Process_Name_Length);
            Mode              : Process_Mode;
            Pid               : Starlet.Process_Id_Type;
            State             : Process_State;
            Curr_Priority,
            Base_Priority     : System.Unsigned_Longword;
            CPU_Time          : System.Unsigned_Longword;
            Terminal          : Text(Max_Terminal_Length);
            Process_Pages,
            Global_Pages      : System.Unsigned_Longword;
            Free_P0_Address,
            Free_P1_Address   : System.Address;
            Images_Activated  : System.Unsigned_Longword;
            Page_Faults       : System.Unsigned_Longword;
            Buffered_IO,
            Direct_IO         : System.Unsigned_Longword;
            Login_Time        : Starlet.Date_Time_Type;
            Image_Name        : Text(Max_Image_Name_Length);
         end record;

      function Physical_Pages (Of_Process : Process_Info) return Natural;

      function Virtual_P0_Pages (Of_Process : Process_Info) return Natural;
      function Virtual_P1_Pages (Of_Process : Process_Info) return Natural;
      function Virtual_Pages    (Of_Process : Process_Info) return Natural;

      function Trimmed_Image_Name (Of_Process : Process_Info) return String;
         -- returns the image file name, type and version

      function "<" (P1, P2 : Process_Info) return Boolean;

   private

      for Process_State'Size use Storage_Units.Longword_Bits;

      for Process_State use (COLPG  => 16#1#, MWAIT  => 16#2#,
                             CEF    => 16#3#, PFW    => 16#4#,
                             LEF    => 16#5#, LEFO   => 16#6#,
                             HIB    => 16#7#, HIBO   => 16#8#,
                             SUSP   => 16#9#, SUSPO  => 16#A#,
                             FPG    => 16#B#, COM    => 16#C#,
                             COMO   => 16#D#, CUR    => 16#E#);

      for Process_Mode'Size use Storage_Units.Longword_Bits;

      for Process_Mode use (Detached    => Starlet.JPI_K_Other,
                            Network     => Starlet.JPI_K_Network,
                            Batch       => Starlet.JPI_K_Batch,
                            Interactive => Starlet.JPI_K_Interactive);

      pragma Inline("<", Physical_Pages,
                    Virtual_P0_Pages, Virtual_P1_Pages, Virtual_Pages);

   end Process_Info_Type;

   use Process_Info_Type;


   package Process_Sets is new General_Sets(Element_Type => Process_Info,
                                            Count        => Natural);

   use Process_Sets;


   type String_Access is access String;

   function "<" (Left, Right : String_Access) return Boolean;

   pragma Inline("<");

   package String_Sets is new General_Sets(Element_Type => String_Access,
                                           Count        => Natural);

   use String_Sets;


   type Process_Mode_Set is array (Process_Mode) of Boolean;


   type Field_Id is (Username, Node, Process_Name,
                     Mode, Pid, State, Priority,
                     CPU_Time, Terminal,
                     Physical_Pages, P_Physical_Pages, G_Physical_Pages,
                     Virtual_Pages, P0_Virtual_Pages, P1_Virtual_Pages,
                     Images_Activated, Page_Faults,
                     IO_Buffered, IO_Direct,
                     Login_Time, Image_Name);

   type Field_Id_Set is array (Field_Id) of Boolean;


   type Field_Id_Array is array (Positive range <>) of Field_Id;

   subtype Field_Count is Natural range 0..Field_Id_Set'Length;

   type Sorting_Order (N_Fields : Field_Count := 0) is
      record
         Fields : Field_Id_Array(1..N_Fields);
      end record;


   type Print_Mode is
      record
         Modes      : Process_Mode_Set := (others => False);
         Cluster    : Boolean := False;
         Nodes      : String_Sets.Set;
         Users      : String_Sets.Set;
         Fields     : Field_Id_Set := (others => False);
         Sort_Order : Sorting_Order;
      end record;

   Field_Width : constant array (Field_Id range Username..Login_Time) of Natural :=
                 (Username         => Max_Username_Length,
                  Node             => Max_Node_Name_Length,
                  Process_Name     => Max_Process_Name_Length,
                  Mode             => 1,
                  Pid              => 8,
                  State            => Process_State'Width,
                  Priority         => 5,
                  CPU_Time         => 14,
                  Terminal         => Max_Terminal_Length,
                  Physical_Pages |
                  P_Physical_Pages |
                  G_Physical_Pages => 6,
                  Virtual_Pages |
                  P0_Virtual_Pages |
                  P1_Virtual_Pages => 6,
                  Images_Activated => 6,
                  IO_Buffered |
                  IO_Direct        => 9,
                  Page_Faults      => 9,
                  Login_Time       => 23);


   Return_Status        : Condition_Handling.Cond_Value_Type;
   IO_Status_Block      : Starlet.IOSB_Type;

   Curr_Pid             : Starlet.Process_Id_Type := 0;
   Curr_Process         : Process_Info;
   pragma Volatile(Curr_Process);

   Processes            : Process_Sets.Set;

   N_Processes          : array (Process_Mode) of Natural := (others => 0);
   Total_Processes,
   Suspended_Processes  : Natural := 0;
   Pr_Mode              : Print_Mode;

   Max_JPI_Items        : constant Positive := 2 * Field_Id_Set'Length;
   JPI_Item_List        : Starlet.Item_List_Type(1..Max_JPI_Items);
   Last_JPI_Item        : Natural range 0..JPI_Item_List'Last;


   function Image is new Number_Images.Integer_Image(Natural);

   function "=" (Left, Right : System.Unsigned_Longword) return Boolean
      renames System."=";


   procedure Check_Condition (Status : in Condition_Handling.Cond_Value_Type) is
   begin
      if not Condition_Handling.Success(Status) then
         Condition_Handling.Signal(Status);
      end if;
   end Check_Condition;

   pragma Inline(Check_Condition);


   procedure Put (Item : in String; Width : in Text_IO.Field := 0) is
   begin
      for I in Item'Range loop
         if Character_Handler.Is_Graphic(Item(I)) then
            Text_IO.Put(Item(I));
         else
            Text_IO.Put('.');
         end if;
      end loop;
      Text_IO.Put((1..Width - Item'Length => ' '));
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      Put(Item);
      Text_IO.New_Line;
   end Put_Line;


   package body Text_Type is
   ----------------------

      function Equal (T1, T2 : Text) return Boolean is
      begin
         return T1.Value(1..T1.Length) = T2.Value(1..T2.Length);
      end;

      function "<" (T1, T2 : Text) return Boolean is
      begin
         return T1.Value(1..T1.Length) < T2.Value(1..T2.Length);
      end "<";

      procedure Assign (T : out Text; S : in String) is
      begin
         T.Length := S'Length;
         T.Value(1..S'Length) := Text_String(S);
      end;

      function To_String (T : Text) return String is
      begin
         return String(T.Value(1..T.Length));
      end;

      procedure Put (Item : in Text; Width : in Text_IO.Field := 0) is
      begin
         Put(String(Item.Value(1..Item.Length)), Width);
      end;

   end Text_Type;


   package body Process_Info_Type is
   ------------------------------

      First_P0_Address  : constant System.Address := System.To_Address(16#00000000#);
      Last_P1_Address   : constant System.Address := System.To_Address(16#7FFFFFFF#);
      Page_Size         : constant := 512;

      use System;  -- operators


      function "<" (P1, P2 : Process_Info) return Boolean is

         function "<" (Left, Right : Starlet.Date_Time_Type) return Boolean is

            Left_Julian_Day,
            Right_Julian_Day  : System.Unsigned_Longword;

            Difference        : Starlet.Date_Time_Type;
            Return_Status     : Condition_Handling.Cond_Value_Type;

         begin
            -- Compare Julian day numbers first since delta times are 9999
            -- days at most and some processes such as SWAPPER are created
            -- on 17-NOV-1858 00:00:00.00
            Lib.Cvt_From_Internal_Time(Status         => Return_Status,
                                       Operation      => Lib.K_Julian_Date,
                                       Resultant_Time => Left_Julian_Day,
                                       Input_Time     => Left);
            Check_Condition(Return_Status);
            Lib.Cvt_From_Internal_Time(Status         => Return_Status,
                                       Operation      => Lib.K_Julian_Date,
                                       Resultant_Time => Right_Julian_Day,
                                       Input_Time     => Right);
            Check_Condition(Return_Status);
            if Left_Julian_Day /= Right_Julian_Day then
               return Left_Julian_Day < Right_Julian_Day;
            else
               -- use Lib$Sub_Times when Julian days are equal
               Lib.Sub_Times(Status         => Return_Status,
                             Time1          => Left,
                             Time2          => Right,
                             Resultant_Time => Difference);
               case Return_Status is
                  when Lib.Normal =>
                     return False;
                  when Lib.NegTim =>
                     return True;
                  when others =>
                     Check_Condition(Return_Status);
               end case;
            end if;
         end "<";

      begin
         for I in Pr_Mode.Sort_Order.Fields'Range loop
            case Pr_Mode.Sort_Order.Fields(I) is
               when Username =>
                  if not Equal(P1.Username, P2.Username) then
                     return P1.Username < P2.Username;
                  end if;
               when Node =>
                  if not Equal(P1.Node_Name, P2.Node_Name) then
                     return P1.Node_Name < P2.Node_Name;
                  end if;
               when Process_Name =>
                  if not Equal(P1.Name, P2.Name) then
                     return P1.Name < P2.Name;
                  end if;
               when Mode =>
                  if P1.Mode /= P2.Mode then
                     return P1.Mode < P2.Mode;
                  end if;
               when Pid =>
                  if P1.Pid /= P2.Pid then
                     return P1.Pid < P2.Pid;
                  end if;
               when State =>
                  if P1.State /= P2.State then
                     return P1.State < P2.State;
                  end if;
               when Priority =>
                  if P1.Base_Priority /= P2.Base_Priority then
                     return P2.Base_Priority < P1.Base_Priority;
                  elsif P1.Curr_Priority /= P2.Curr_Priority then
                     return P2.Curr_Priority < P1.Curr_Priority;
                  end if;
               when CPU_Time =>
                  if P1.CPU_Time /= P2.CPU_Time then
                     return P2.CPU_Time < P1.CPU_Time;
                  end if;
               when Terminal =>
                  if not Equal(P1.Terminal, P2.Terminal) then
                     return P1.Terminal < P2.Terminal;
                  end if;
               when Physical_Pages =>
                  if Physical_Pages(P1) /= Physical_Pages(P2) then
                     return Physical_Pages(P2) < Physical_Pages(P1);
                  end if;
               when P_Physical_Pages =>
                  if P1.Process_Pages /= P2.Process_Pages then
                     return P2.Process_Pages < P1.Process_Pages;
                  end if;
               when G_Physical_Pages =>
                  if P1.Global_Pages /= P2.Global_Pages then
                     return P2.Global_Pages < P1.Global_Pages;
                  end if;
               when Virtual_Pages =>
                  if Virtual_Pages(P1) /= Virtual_Pages(P2) then
                     return Virtual_Pages(P2) < Virtual_Pages(P1);
                  end if;
               when P0_Virtual_Pages =>
                  if Virtual_P0_Pages(P1) /= Virtual_P0_Pages(P2) then
                     return Virtual_P0_Pages(P2) < Virtual_P0_Pages(P1);
                  end if;
               when P1_Virtual_Pages =>
                  if Virtual_P1_Pages(P1) /= Virtual_P1_Pages(P2) then
                     return Virtual_P1_Pages(P2) < Virtual_P1_Pages(P1);
                  end if;
               when Images_Activated =>
                  if P1.Images_Activated /= P2.Images_Activated then
                     return P2.Images_Activated < P1.Images_Activated;
                  end if;
               when Page_Faults =>
                  if P1.Page_Faults /= P2.Page_Faults then
                     return P2.Page_Faults < P1.Page_Faults;
                  end if;
               when IO_Buffered =>
                  if P1.Buffered_IO /= P2.Buffered_IO then
                     return P2.Buffered_IO < P1.Buffered_IO;
                  end if;
               when IO_Direct =>
                  if P1.Direct_IO /= P2.Direct_IO then
                     return P2.Direct_IO < P1.Direct_IO;
                  end if;
               when Login_Time =>
                  if P1.Login_Time /= P2.Login_Time then
                     return P2.Login_Time < P1.Login_Time;
                  end if;
               when Image_Name =>
                  if Trimmed_Image_Name(P1) /= Trimmed_Image_Name(P2) then
                     return Trimmed_Image_Name(P1) < Trimmed_Image_Name(P2);
                  end if;
            end case;
         end loop;
         -- Compare Pids when all selected fields are equal
         return P1.Pid < P2.Pid;
      end "<";


      function Physical_Pages (Of_Process : Process_Info) return Natural is
      begin
         return Natural(Of_Process.Process_Pages) + Natural(Of_Process.Global_Pages);
      end;


      function Virtual_P0_Pages (Of_Process : Process_Info) return Natural is

         use System;  -- operators

      begin
         return (Of_Process.Free_P0_Address - First_P0_Address) / Page_Size;
      end;

      function Virtual_P1_Pages (Of_Process : Process_Info) return Natural is

         use System;  -- operators

      begin
         return (Last_P1_Address - Of_Process.Free_P1_Address) / Page_Size;
      end;

      function Virtual_Pages (Of_Process : Process_Info) return Natural is
      begin
         return Virtual_P0_Pages(Of_Process) + Virtual_P1_Pages(Of_Process);
      end;


      function Trimmed_Image_Name (Of_Process : Process_Info) return String is

         Proc_Image_Name : constant String := To_String(Of_Process.Image_Name);

         function Max is new Min_Max_Functions.Maximum(Integer);

         First : constant Integer range 1..Proc_Image_Name'Last + 1 :=
                 Max(String_Handler.Locate(Pattern => ']',
                                           Within  => Proc_Image_Name,
                                           From    => String_Handler.Right),
                     String_Handler.Locate(Pattern => ':',
                                           Within  => Proc_Image_Name,
                                           From    => String_Handler.Right)) + 1;

      begin
         return Proc_Image_Name(First..Proc_Image_Name'Last);
      end Trimmed_Image_Name;

   end Process_Info_Type;


   function "<" (Left, Right : String_Access) return Boolean is
   begin
      return Left.all < Right.all;
   end;


   function Hexa (N : System.Unsigned_Longword) return String is

      N_Digits  : constant := System.Unsigned_Longword'Size / 4;

      type Hex_Array is array (Integer range 0..N_Digits - 1) of Character_Handler.Digit_Number;
      pragma Pack(Hex_Array);
      for Hex_Array'Size use System.Unsigned_Longword'Size;

      function To_Hex_Array is new Unchecked_Conversion(Source => System.Unsigned_Longword,
                                                        Target => Hex_Array);

      Str        : String(1..N_Digits);
      Hex_Slices : constant Hex_Array := To_Hex_Array(N);

   begin
      for I in Integer range 0..N_Digits - 1 loop
         Str(N_Digits - I) := Character_Handler.Digit_Image(Hex_Slices(I));
      end loop;
      return Str;
   end Hexa;

   pragma Inline(Hexa);


   procedure Get_Command_Line (Pr_Mode : in out Print_Mode) is

      Return_Status   : Condition_Handling.Cond_Value_Type;

      Value           : String(1..100);
      pragma Volatile(Value);

      Length          : Natural range 0..Value'Length;
      pragma Volatile(Length);

      Defaulted_Modes : Process_Mode_Set := (others => False);

   begin
      loop
         CLI.Get_Value(Status      => Return_Status,
                       Entity_Desc => "USERNAME",
                       Retdesc     => Value,
                       Retlength   => System.Unsigned_Word(Length));
         exit when Return_Status = CLI.Absent;
         if Return_Status /= CLI.Comma then
            Check_Condition(Return_Status);
         end if;
         Add(Element => new String'(Value(1..Length)),
             To      => Pr_Mode.Users);
      end loop;
      --
      CLI.Present(Status      => Return_Status,
                  Entity_Desc => "ALL");
      case Return_Status is
         when CLI.Presen | CLI.Defaulted =>
            Pr_Mode.Modes := (others => True);
         when CLI.Absent | CLI.Negated =>
            Pr_Mode.Modes := (others => False);
         when others =>
            Check_Condition(Return_Status);
      end case;
      --
      for Mode in Process_Mode loop
         CLI.Present(Status      => Return_Status,
                     Entity_Desc => Process_Mode'Image(Mode));
         case Return_Status is
            when CLI.Presen =>
               Pr_Mode.Modes(Mode) := True;
            when CLI.Defaulted =>
               Defaulted_Modes(Mode) := True;
            when CLI.Absent =>
               null;
            when CLI.Negated =>
               Pr_Mode.Modes(Mode) := False;
            when others =>
               Check_Condition(Return_Status);
         end case;
      end loop;
      if Pr_Mode.Modes = (Pr_Mode.Modes'Range => False) then
         Pr_Mode.Modes := Defaulted_Modes;
      end if;
      --
      loop
         CLI.Get_Value(Status      => Return_Status,
                       Entity_Desc => "NODE",
                       Retdesc     => Value,
                       Retlength   => System.Unsigned_Word(Length));
         exit when Return_Status = CLI.Absent;
         if Return_Status /= CLI.Comma then
            Check_Condition(Return_Status);
         end if;
         Add(Element => new String'(Value(1..Length)),
             To      => Pr_Mode.Nodes);
      end loop;
      --
      CLI.Present(Status      => Return_Status,
                  Entity_Desc => "CLUSTER");
      case Return_Status is
         when CLI.Presen | CLI.Defaulted =>
            Pr_Mode.Cluster := True;
         when CLI.Absent | CLI.Negated =>
            Pr_Mode.Cluster := False;
         when others =>
            Check_Condition(Return_Status);
      end case;
      --
      loop
         CLI.Get_Value(Status      => Return_Status,
                       Entity_Desc => "SHOW",
                       Retdesc     => Value,
                       Retlength   => System.Unsigned_Word(Length));
         exit when Return_Status = CLI.Absent;
         if Return_Status /= CLI.Comma then
            Check_Condition(Return_Status);
         end if;
         for Field in Field_Id loop
            if Length <= Field_Id'Image(Field)'Length and then
               Field_Id'Image(Field)(1..Length) = Value(1..Length)
            then
               if not Pr_Mode.Fields(Field) then
                  Pr_Mode.Fields(Field) := True;
                  Pr_Mode.Sort_Order := (N_Fields => Pr_Mode.Sort_Order.N_Fields + 1,
                                         Fields   => Pr_Mode.Sort_Order.Fields & Field);
               end if;
            end if;
         end loop;
      end loop;
   end Get_Command_Line;


   procedure Call_Process_Scan (Pr_Mode     : in Print_Mode;
                                Pid_Context : in out Starlet.Process_Id_Type) is

      function "or"  (Left, Right : System.Unsigned_Longword) return System.Unsigned_Longword renames System."or";
      function "and" (Left, Right : System.Unsigned_Longword) return System.Unsigned_Longword renames System."and";
      function "not" (Right       : System.Unsigned_Longword) return System.Unsigned_Longword renames System."not";


      Process_Scan_Item_List : Starlet.Item_List_Type(1..Card(Pr_Mode.Users) +
                                                         Card(Pr_Mode.Nodes) + 3);

      Last_Item : Natural range 0..Process_Scan_Item_List'Last := 0;

      procedure Add_To_Item_List (Item : in Starlet.Item_Rec_Type) is
      begin
         Last_Item := Last_Item + 1;
         Process_Scan_Item_List(Last_Item) := Item;
      end;


      procedure Add_User_To_Item_List (User_Name : in String_Access) is
      begin
         Add_To_Item_List(Item => (Buf_Len     => User_Name'Length,
                                   Item_Code   => Starlet.Pscan_Username,
                                   Buf_Address => User_Name.all'Address,
                                   Ret_Address => System.To_Address(Starlet.Pscan_M_Or or
                                                                    Starlet.Pscan_M_Prefix_Match or
                                                                    Starlet.Pscan_M_Case_Blind)));
      end;

      procedure Add_Node_To_Item_List (Node_Name : in String_Access) is
      begin
         Add_To_Item_List(Item => (Buf_Len     => Node_Name'Length,
                                   Item_Code   => Starlet.Pscan_Nodename,
                                   Buf_Address => Node_Name.all'Address,
                                   Ret_Address => System.To_Address(Starlet.Pscan_M_Or or
                                                                    Starlet.Pscan_M_Wildcard or
                                                                    Starlet.Pscan_M_Case_Blind)));
      end;


      procedure Add_Users_To_Item_List is
         new String_Sets.Enumeration(Action => Add_User_To_Item_List);

      procedure Add_Nodes_To_Item_List is
         new String_Sets.Enumeration(Action => Add_Node_To_Item_List);

   begin
      if Pr_Mode.Cluster then
         -- item for listing all nodes in the cluster
         Add_To_Item_List(Item => (Buf_Len     => 0,
                                   Item_Code   => Starlet.Pscan_Node_Csid,
                                   Buf_Address => System.To_Address(0),
                                   Ret_Address => System.To_Address(Starlet.Pscan_M_Neq)));
      end if;
      -- buffer for enhanced performance
      Add_To_Item_List(Item => (Buf_Len     => 0,
                                Item_Code   => Starlet.Pscan_GetJPI_Buffer_Size,
                                Buf_Address => System.To_Address(1500),
                                Ret_Address => System.Address_Zero));
      -- requested users
      Add_Users_To_Item_List(Pr_Mode.Users);
      Process_Scan_Item_List(Last_Item).Ret_Address :=                              -- the last username
         System.To_Address(System.To_Unsigned_Longword                              -- item must not have
                              (Process_Scan_Item_List(Last_Item).Ret_Address) and   -- the 'or' flag
                           not Starlet.Pscan_M_Or);
      -- requested nodes
      Add_Nodes_To_Item_List(Pr_Mode.Nodes);
      Process_Scan_Item_List(Last_Item).Ret_Address :=
         System.To_Address(System.To_Unsigned_Longword                              -- the last nodename
                              (Process_Scan_Item_List(Last_Item).Ret_Address) and   -- item must not have
                           not Starlet.Pscan_M_Or);                                         -- the 'or' flag
      -- terminate the item list
      Add_To_Item_List(Item => (Buf_Len     => 0,
                                Item_Code   => 0,
                                Buf_Address => System.Address_Zero,
                                Ret_Address => System.Address_Zero));
      --
      Starlet.Process_Scan(Status => Return_Status,
                           Pidctx => Pid_Context,
                           Itmlst => Process_Scan_Item_List(1..Last_Item));
      Check_Condition(Return_Status);
   end Call_Process_Scan;


   procedure Build_JPI_Item_List (Fields    : in Field_Id_Set;
                                  Item_List : out Starlet.Item_List_Type;
                                  Last_Item : out Natural) is

      function "/" (Left, Right : System.Unsigned_Word) return System.Unsigned_Word
         renames System."/";

      Last : Natural range Item_List'First - 1..Item_List'Last :=
             Item_List'First - 1;

      procedure Add_To_Item_List (New_Item : in Starlet.Item_Rec_Type) is
      begin
         Last := Last + 1;
         Item_List(Last) := New_Item;
      end Add_To_Item_List;


      use Storage_Units;

   begin
      if Fields(Username) then
         Add_To_Item_List((Buf_Len     => System.Unsigned_Word(Curr_Process.Username.Max_Length),
                           Item_Code   => Starlet.JPI_Username,
                           Buf_Address => Curr_Process.Username.Value'Address,
                           Ret_Address => Curr_Process.Username.Length'Address));
      end if;
      if Fields(Node) then
         Add_To_Item_List((Buf_Len     => System.Unsigned_Word(Curr_Process.Node_Name.Max_Length),
                           Item_Code   => Starlet.JPI_Nodename,
                           Buf_Address => Curr_Process.Node_Name.Value'Address,
                           Ret_Address => Curr_Process.Node_Name.Length'Address));
      end if;
      if Fields(Process_Name) then
         Add_To_Item_List((Buf_Len     => System.Unsigned_Word(Curr_Process.Name.Max_Length),
                           Item_Code   => Starlet.JPI_Prcnam,
                           Buf_Address => Curr_Process.Name.Value'Address,
                           Ret_Address => Curr_Process.Name.Length'Address));
      end if;
      -- Mode is always included in order to be able to compute the summary
      Add_To_Item_List((Buf_Len     => Curr_Process.Mode'Size / Byte_Bits,
                        Item_Code   => Starlet.JPI_Mode,
                        Buf_Address => Curr_Process.Mode'Address,
                        Ret_Address => System.Address_Zero));
      -- Pid is always included so that all processes are different
      Add_To_Item_List((Buf_Len     => Curr_Process.Pid'Size / Byte_Bits,
                        Item_Code   => Starlet.JPI_Pid,
                        Buf_Address => Curr_Process.Pid'Address,
                        Ret_Address => System.Address_Zero));
      if Fields(State) then
         Add_To_Item_List((Buf_Len     => Curr_Process.State'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_State,
                           Buf_Address => Curr_Process.State'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(Priority) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Base_Priority'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_Prib,
                           Buf_Address => Curr_Process.Base_Priority'Address,
                           Ret_Address => System.Address_Zero));
         Add_To_Item_List((Buf_Len     => Curr_Process.Curr_Priority'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_Pri,
                           Buf_Address => Curr_Process.Curr_Priority'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(CPU_Time) then
         Add_To_Item_List((Buf_Len     => Curr_Process.CPU_Time'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_CPUTim,
                           Buf_Address => Curr_Process.CPU_Time'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(Terminal) then
         Add_To_Item_List((Buf_Len     => System.Unsigned_Word(Curr_Process.Terminal.Max_Length),
                           Item_Code   => Starlet.JPI_Terminal,
                           Buf_Address => Curr_Process.Terminal.Value'Address,
                           Ret_Address => Curr_Process.Terminal.Length'Address));
      end if;
      if Fields(P_Physical_Pages) or Fields(Physical_Pages) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Process_Pages'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_PPgCnt,
                           Buf_Address => Curr_Process.Process_Pages'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(G_Physical_Pages) or Fields(Physical_Pages) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Global_Pages'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_GPgCnt,
                           Buf_Address => Curr_Process.Global_Pages'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(P0_Virtual_Pages) or Fields(Virtual_Pages) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Free_P0_Address'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_FreP0VA,
                           Buf_Address => Curr_Process.Free_P0_Address'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(P1_Virtual_Pages) or Fields(Virtual_Pages) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Free_P1_Address'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_FreP1VA,
                           Buf_Address => Curr_Process.Free_P1_Address'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(Images_Activated) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Images_Activated'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_ImageCount,
                           Buf_Address => Curr_Process.Images_Activated'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(Page_Faults) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Page_Faults'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_PageFlts,
                           Buf_Address => Curr_Process.Page_Faults'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(IO_Buffered) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Buffered_IO'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_BufIO,
                           Buf_Address => Curr_Process.Buffered_IO'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(IO_Direct) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Direct_IO'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_DirIO,
                           Buf_Address => Curr_Process.Direct_IO'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(Login_Time) then
         Add_To_Item_List((Buf_Len     => Curr_Process.Login_Time'Size / Byte_Bits,
                           Item_Code   => Starlet.JPI_LoginTim,
                           Buf_Address => Curr_Process.Login_Time'Address,
                           Ret_Address => System.Address_Zero));
      end if;
      if Fields(Image_Name) then
         Add_To_Item_List((Buf_Len     => System.Unsigned_Word(Curr_Process.Image_Name.Max_Length),
                           Item_Code   => Starlet.JPI_Imagname,
                           Buf_Address => Curr_Process.Image_Name.Value'Address,
                           Ret_Address => Curr_Process.Image_Name.Length'Address));
      end if;
      Add_To_Item_List((Buf_Len     => 0,
                        Item_Code   => 0,
                        Buf_Address => System.Address_Zero,
                        Ret_Address => System.Address_Zero));
      Last_Item := Last;
   end Build_JPI_Item_List;


   procedure Put_Title is

      Date_Time : Text(Max_Time_Image_Length);

      function Label (Of_Field : Field_Id) return String is
      begin
         case Of_Field is
            when Username =>
               return "Username";
            when Node =>
               return "Node";
            when Process_Name =>
               return "Process name";
            when Mode =>
               return "M";
            when Pid =>
               return "Pid";
            when State =>
               return "State";
            when Priority =>
               return " Pri";
            when CPU_Time =>
               return " CPU time";
            when Terminal =>
               return "Terminal";
            when Physical_Pages =>
               return "Phys P";
            when P_Physical_Pages =>
               return "Prc Pg";
            when G_Physical_Pages =>
               return "Glo Pg";
            when Virtual_Pages =>
               return "Virt P";
            when P0_Virtual_Pages =>
               return "Vir P0";
            when P1_Virtual_Pages =>
               return "Vir P1";
            when Images_Activated =>
               return "Images";
            when Page_Faults =>
               return "Page flts";
            when IO_Buffered =>
               return "  Buf IO";
            when IO_Direct =>
               return "  Dir IO";
            when Login_Time =>
               return "     Login time";
            when Image_Name =>
               return "Image name";
         end case;
      end Label;

   begin
      -- get the system time
      Starlet.AscTim(Status => Return_Status,
                     Timlen => System.Unsigned_Word(Date_Time.Length),
                     Timbuf => Starlet.Time_Name_Type(Date_Time.Value));
      Check_Condition(Return_Status);
      Put_Line("                 System state : " & To_String(Date_Time));
      for The_Field in Field_Width'Range loop
         if Pr_Mode.Fields(The_Field) then
            Put(Label(The_Field), Width => Field_Width(The_Field) + 1);
         end if;
      end loop;
      if Pr_Mode.Fields(Image_Name) then
         Put(Label(Image_Name));
      end if;
      Text_IO.New_Line;
   end Put_Title;


   procedure Put_Process (Proc : in Process_Info) is

      function Image is new Number_Images.Integer_Image(System.Unsigned_Longword);

   begin
      if Pr_Mode.Fields(Username) then
         Put(Proc.Username, Width => Field_Width(Username) + 1);
      end if;
      if Pr_Mode.Fields(Node) then
         Put(Proc.Node_Name, Width => Field_Width(Node) + 1);
      end if;
      if Pr_Mode.Fields(Process_Name) then
         Put(Proc.Name, Width => Field_Width(Process_Name) + 1);
      end if;
      if Pr_Mode.Fields(Mode) then
         case Proc.Mode is
            when Detached =>
               Put(" ", Width => Field_Width(Mode) + 1);
            when Network =>
               Put("N", Width => Field_Width(Mode) + 1);
            when Batch =>
               Put("B", Width => Field_Width(Mode) + 1);
            when Interactive =>
               Put("I", Width => Field_Width(Mode) + 1);
         end case;
      end if;
      if Pr_Mode.Fields(Pid) then
         Put(Hexa(Proc.Pid), Width => Field_Width(Pid) + 1);
      end if;
      if Pr_Mode.Fields(State) then
         Put(Process_State'Image(Proc.State), Width => Field_Width(State) + 1);
      end if;
      if Pr_Mode.Fields(Priority) then
         Put(Image(Proc.Curr_Priority, Width => 2));
         Put("/");
         Put(Image(Proc.Base_Priority), Width => 2);
         Put(" ");
      end if;
      if Pr_Mode.Fields(CPU_Time) then
         declare

            Second : constant := 100;
            Minute : constant := 60 * Second;
            Hour   : constant := 60 * Minute;
            Day    : constant := 24 * Hour;


            function Zero_Padded (Str : String) return String is

               Result : String(Str'Range);

            begin
               for I in Str'Range loop
                  if Str(I) = ' ' then
                     Result(I) := '0';
                  else
                     Result(I) := Str(I);
                  end if;
               end loop;
               return Result;
            end Zero_Padded;


            use System;  -- operators

         begin
            Put(Image(Proc.CPU_Time / Day, Width => 2) & ' ' &
                Zero_Padded(Image(Proc.CPU_Time mod Day    / Hour,   Width => 2)) & ':' &
                Zero_Padded(Image(Proc.CPU_Time mod Hour   / Minute, Width => 2)) & ':' &
                Zero_Padded(Image(Proc.CPU_Time mod Minute / Second, Width => 2)) & '.' &
                Zero_Padded(Image(Proc.CPU_Time mod Second,          Width => 2)));
         end;
         Put(" ");
      end if;
      if Pr_Mode.Fields(Terminal) then
         Put(Proc.Terminal, Width => Field_Width(Terminal) + 1);
      end if;
      if Pr_Mode.Fields(Physical_Pages) then
         Put(Image(Physical_Pages(Proc),
                   Width => Field_Width(Physical_Pages)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(P_Physical_Pages) then
         Put(Image(Proc.Process_Pages,
                   Width => Field_Width(P_Physical_Pages)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(G_Physical_Pages) then
         Put(Image(Proc.Global_Pages,
                   Width => Field_Width(G_Physical_Pages)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(Virtual_Pages) then
         Put(Image(Virtual_Pages(Proc),
                   Width => Field_Width(Virtual_Pages)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(P0_Virtual_Pages) then
         Put(Image(Virtual_P0_Pages(Proc),
                   Width => Field_Width(P0_Virtual_Pages)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(P1_Virtual_Pages) then
         Put(Image(Virtual_P1_Pages(Proc),
                   Width => Field_Width(P1_Virtual_Pages)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(Images_Activated) then
         Put(Image(Proc.Images_Activated, Width => Field_Width(Images_Activated)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(Page_Faults) then
         Put(Image(Proc.Page_Faults, Width => Field_Width(Page_Faults)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(IO_Buffered) then
         Put(Image(Proc.Buffered_IO, Width => Field_Width(IO_Buffered)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(IO_Direct) then
         Put(Image(Proc.Direct_IO, Width => Field_Width(IO_Direct)));
         Put(" ");
      end if;
      if Pr_Mode.Fields(Login_Time) then
         declare

            Login_Time_Image : Text(Max_Time_Image_Length);

         begin
            Starlet.AscTim(Status => Return_Status,
                           Timlen => System.Unsigned_Word(Login_Time_Image.Length),
                           Timbuf => Starlet.Time_Name_Type(Login_Time_Image.Value),
                           TimAdr => Proc.Login_Time);
            Check_Condition(Return_Status);
            Put(Login_Time_Image, Width => Field_Width(Login_Time) + 1);
         end;
      end if;
      if Pr_Mode.Fields(Image_Name) then
         Put(Trimmed_Image_Name(Proc));
      end if;
      Text_IO.New_Line;
   end Put_Process;

   procedure Put_Processes is
      new Process_Sets.Enumeration(Action => Put_Process);


   procedure Put_Summary is
   begin
      Put_Line("     Processes: "   & Image(Total_Processes) &
               "     Interactive: " & Image(N_Processes(Interactive)) &
               "     Batch: "       & Image(N_Processes(Batch)) &
               "     Network: "     & Image(N_Processes(Network)));
   end Put_Summary;


begin
   Get_Command_Line(Pr_Mode);
   Put_Title;
   Call_Process_Scan(Pr_Mode,
                     Pid_Context => Curr_Pid);
   Build_JPI_Item_List(Fields    => Pr_Mode.Fields,
                       Item_List => JPI_Item_List,
                       Last_Item => Last_JPI_Item);
   -- collect process information
   loop
      Starlet.GetJPIW(Status => Return_Status,
                      Pidadr => Curr_Pid,
                      Itmlst => JPI_Item_List(1..Last_JPI_Item),
                      IOSB   => IO_Status_Block);
      if Condition_Handling.Success(Return_Status) then
         Return_Status := Condition_Handling.Cond_Value_Type(IO_Status_Block.Status);
      end if;
      exit when Return_Status = Starlet.SS_Nomoreproc;
      if Return_Status = Starlet.SS_Normal then
         N_Processes(Curr_Process.Mode) := N_Processes(Curr_Process.Mode) + 1;
         Total_Processes := Total_Processes + 1;
         if Pr_Mode.Modes(Curr_Process.Mode) then
            Add(Curr_Process, To => Processes);
         end if;
      elsif Return_Status = Starlet.SS_Suspended then
         Suspended_Processes := Suspended_Processes + 1;
      else
         Check_Condition(Return_Status);
      end if;
   end loop;
   -- display processes
   Put_Processes(Processes);
   if Suspended_Processes > 0 then
      Put("-- The requested information was not available for ");
      Put(Image(Suspended_Processes));
      Put(" process");
      if Suspended_Processes > 1 then
         Put("es");
      end if;
      Put(" --");
      Text_IO.New_Line;
   end if;
   Put_Summary;
end State;
