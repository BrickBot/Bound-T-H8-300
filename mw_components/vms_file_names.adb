-- VMS FILE NAME HANDLER
   ---------------------

-- Creation : 20-SEP-1986 by Mats Weber.


with System,
     Condition_Handling,
     IO_Exceptions,
     VMS_Logical_Names;

package body VMS_File_Names is
---------------------------

  procedure Check_Condition (Status : in Condition_Handling.Cond_Value_Type) is
    -- Checks STATUS and raises the appropriate exception
  begin
    if Condition_Handling.Match_Cond(Status,
                                     Starlet.RMS_Nod,
                                     Starlet.RMS_Dev,
                                     Starlet.RMS_Dir,
                                     Starlet.RMS_Fnm,
                                     Starlet.RMS_Typ,
                                     Starlet.RMS_Ver,
                                     Starlet.RMS_Syn) /= 0
    then
      raise IO_Exceptions.Name_Error;
    end if;
    if not Condition_Handling.Success(Status) then
      raise File_Error;
    end if;
  end Check_Condition;


  function File_Name_String (File_Specification : File_Name_Fields) return String is
  begin
    return To_String(File_Specification.Node_Name) &
           To_String(File_Specification.Device) &
           To_String(File_Specification.Directory) &
           To_String(File_Specification.File_Name) &
           To_String(File_Specification.File_Type) &
           To_String(File_Specification.Version);
  end File_Name_String;


  function Full_File_Name (File_Name      : String;
                           Default        : String := "";
                           Alter_Password : Boolean := False) return String is

    Expanded_File_Name : String(1..Starlet.Nam_C_MaxRSS);
    Fab_Block          : Starlet.Fab_Type := Starlet.Fab_Type_Init;
    Nam_Block          : Starlet.Nam_Type := Starlet.Nam_Type_Init;

    Ret_Status         : Condition_Handling.Cond_Value_Type;

  begin
    Fab_Block.Fna := File_Name'Address;
    Fab_Block.Fns := File_Name'Length;
    Fab_Block.Dna := Default'Address;
    Fab_Block.Dns := Default'Length;
    Fab_Block.Nam := Nam_Block'Address;

    Nam_Block.Esa := Expanded_File_Name'Address;
    Nam_Block.Ess := Expanded_File_Name'Length;
    Nam_Block.Nop.Pwd := not Alter_Password;

    Starlet.Parse(Status => Ret_Status,
                  Fab    => Fab_Block);
    Check_Condition(Ret_Status);
    return Expanded_File_Name(1..Integer(Nam_Block.Esl));
  end Full_File_Name;


  function Sliced_File_Name (Expanded_File_Name : String;
                             Nam_Block          : Starlet.Nam_Type) return File_Name_Fields is

    Index            : Natural := Expanded_File_Name'First;
    Parsed_File_Name : File_Name_Fields;

  begin
    Assign(Parsed_File_Name.Node_Name, Expanded_File_Name(Index..Index + Integer(Nam_Block.B_Node) - 1));
    Index := Index + Integer(Nam_Block.B_Node);
    Assign(Parsed_File_Name.Device,    Expanded_File_Name(Index..Index + Integer(Nam_Block.B_Dev) - 1));
    Index := Index + Integer(Nam_Block.B_Dev);
    Assign(Parsed_File_Name.Directory, Expanded_File_Name(Index..Index + Integer(Nam_Block.B_Dir) - 1));
    Index := Index + Integer(Nam_Block.B_Dir);
    Assign(Parsed_File_Name.File_Name, Expanded_File_Name(Index..Index + Integer(Nam_Block.B_Name) - 1));
    Index := Index + Integer(Nam_Block.B_Name);
    Assign(Parsed_File_Name.File_Type, Expanded_File_Name(Index..Index + Integer(Nam_Block.B_Type) - 1));
    Index := Index + Integer(Nam_Block.B_Type);
    Assign(Parsed_File_Name.Version,   Expanded_File_Name(Index..Index + Integer(Nam_Block.B_Ver) - 1));
    return Parsed_File_Name;
  end Sliced_File_Name;


  function Full_File_Name (File_Name      : String;
                           Default        : String := "";
                           Alter_Password : Boolean := False) return File_Name_Fields is

    Expanded_File_Name : String(1..Starlet.Nam_C_MaxRSS);
    Fab_Block          : Starlet.Fab_Type := Starlet.Fab_Type_Init;
    Nam_Block          : Starlet.Nam_Type := Starlet.Nam_Type_Init;

    Ret_Status         : Condition_Handling.Cond_Value_Type;

  begin
    Fab_Block.Fna := File_Name'Address;
    Fab_Block.Fns := File_Name'Length;
    Fab_Block.Dna := Default'Address;
    Fab_Block.Dns := Default'Length;
    Fab_Block.Nam := Nam_Block'Address;

    Nam_Block.Esa := Expanded_File_Name'Address;
    Nam_Block.Ess := Expanded_File_Name'Length;
    Nam_Block.Nop.Pwd := not Alter_Password;

    Starlet.Parse(Status => Ret_Status,
                  Fab    => Fab_Block);
    Check_Condition(Ret_Status);
    return Sliced_File_Name(Expanded_File_Name, Nam_Block);
  end Full_File_Name;


  function Local_Node_Name return String is
  begin
    return VMS_Logical_Names.Translate(Logical_Name => "SYS$NODE",
                                       Table        => "LNM$SYSTEM",
                                       Iterative    => False);
  end Local_Node_Name;


  procedure Enumerate_Matching_Files (File_Name      : in String;
                                      Default        : in String := "";
                                      Alter_Password : in Boolean := False) is

    Expanded_File_Name : String(1..Starlet.Nam_C_MaxRSS);
    Curr_File_Name     : String(1..Starlet.Nam_C_MaxRSS);
    Fab_Block          : Starlet.Fab_Type := Starlet.Fab_Type_Init;
    Nam_Block          : Starlet.Nam_Type := Starlet.Nam_Type_Init;

    Ret_Status         : Condition_Handling.Cond_Value_Type;

  begin
    Fab_Block.Fna := File_Name'Address;
    Fab_Block.Fns := File_Name'Length;
    Fab_Block.Dna := Default'Address;
    Fab_Block.Dns := Default'Length;
    Fab_Block.Nam := Nam_Block'Address;

    Nam_Block.Esa := Expanded_File_Name'Address;
    Nam_Block.Ess := Expanded_File_Name'Length;
    Nam_Block.Rsa := Curr_File_Name'Address;
    Nam_Block.Rss := Curr_File_Name'Length;
    Nam_Block.Nop.Pwd := not Alter_Password;

    Starlet.Parse(Status => Ret_Status, Fab => Fab_Block);
    Check_Condition(Ret_Status);
    loop
      Starlet.Search(Status => Ret_Status, Fab => Fab_Block);
      exit when Condition_Handling.Match_Cond(Ret_Status,
                                              Starlet.RMS_Nmf,
                                              Starlet.RMS_Fnf) /= 0;
      if not Condition_Handling.Success(Ret_Status) then
        raise File_Error;
      end if;
      Action(Curr_File_Name(1..Integer(Nam_Block.Rsl)));
    end loop;
  end Enumerate_Matching_Files;


  procedure Enumerate_Matching_File_Specifications (File_Name      : in String;
                                                    Default        : in String := "";
                                                    Alter_Password : in Boolean := False) is

    Expanded_File_Name : String(1..Starlet.Nam_C_MaxRSS);
    Curr_File_Name     : String(1..Starlet.Nam_C_MaxRSS);
    Fab_Block          : Starlet.Fab_Type := Starlet.Fab_Type_Init;
    Nam_Block          : Starlet.Nam_Type := Starlet.Nam_Type_Init;

    Ret_Status         : Condition_Handling.Cond_Value_Type;

  begin
    Fab_Block.Fna := File_Name'Address;
    Fab_Block.Fns := File_Name'Length;
    Fab_Block.Dna := Default'Address;
    Fab_Block.Dns := Default'Length;
    Fab_Block.Nam := Nam_Block'Address;

    Nam_Block.Esa := Expanded_File_Name'Address;
    Nam_Block.Ess := Expanded_File_Name'Length;
    Nam_Block.Rsa := Curr_File_Name'Address;
    Nam_Block.Rss := Curr_File_Name'Length;
    Nam_Block.Nop.Pwd := not Alter_Password;

    Starlet.Parse(Status => Ret_Status, Fab => Fab_Block);
    Check_Condition(Ret_Status);
    loop
      Starlet.Search(Status => Ret_Status, Fab => Fab_Block);
      exit when Condition_Handling.Match_Cond(Ret_Status,
                                              Starlet.RMS_Nmf,
                                              Starlet.RMS_Fnf) /= 0;
      if not Condition_Handling.Success(Ret_Status) then
        raise File_Error;
      end if;
      Action(Sliced_File_Name(Curr_File_Name, Nam_Block));
    end loop;
  end Enumerate_Matching_File_Specifications;

end VMS_File_Names;
