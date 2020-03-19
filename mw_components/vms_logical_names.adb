-- VMS LOGICAL NAMES
   -----------------

-- Creation :  2-OCT-1986 by Mats Weber.


with System,
     Starlet,
     Condition_Handling,
     Storage_Units,
     String_Case_Conversions;

use Storage_Units,
    String_Case_Conversions;

package body VMS_Logical_Names is
------------------------------

  function "=" (Left, Right : Condition_Handling.Cond_Value_Type) return Boolean
    renames System."=";


  function Translate (Logical_Name : String;
                      Table        : String  := "LNM$DCL_LOGICAL";
                      Iterative    : Boolean := True) return String is

    Equivalence_Name : String(1..Starlet.Lnm_C_Namlength);

    type Returned_Length is range 0..Starlet.Lnm_C_Namlength;
    for Returned_Length'Size use Word_Bits;

    Equivalence_Name_Length : Returned_Length;

    Return_Status : Condition_Handling.Cond_Value_Type;


    Lnm_Item_List : constant Starlet.Item_List_Type :=
      (1 =>
        (Item_Code   => Starlet.Lnm_String,
         Buf_Len     => Equivalence_Name'Length,
         Buf_Address => Equivalence_Name'Address,
         Ret_Address => Equivalence_Name_Length'Address),
       2 =>
        (Item_Code   => 0,
         Buf_Len     => 0,
         Buf_Address |
         Ret_Address => System.Address_Zero));

  begin
    if not Iterative then
      Starlet.Trnlnm(Status => Return_Status,
                     Tabnam => Upper_Case(Table),
                     Lognam => Upper_Case(Logical_Name),
                     Itmlst => Lnm_Item_List);
      if Return_Status = Starlet.Ss_Nolognam then
        raise Logical_Name_Not_Found;
      elsif not Condition_Handling.Success(Return_Status) then
        raise Logical_Name_Error;
      end if;
      return Equivalence_Name(1..Integer(Equivalence_Name_Length));
    else
      Equivalence_Name(1..Logical_Name'Length) := Upper_Case(Logical_Name);
      Equivalence_Name_Length := Logical_Name'Length;
      loop
        Starlet.Trnlnm(Status => Return_Status,
                       Tabnam => Upper_Case(Table),
                       Lognam => Equivalence_Name(1..Integer(Equivalence_Name_Length)),
                       Itmlst => Lnm_Item_List);
        if Return_Status = Starlet.Ss_Nolognam then
          return Equivalence_Name(1..Integer(Equivalence_Name_Length));
        elsif not Condition_Handling.Success(Return_Status) then
          raise Logical_Name_Error;
        end if;
      end loop;
    end if;
  end Translate;


  procedure Define (Logical_Name,
                    Equivalence_Name : in String;
                    Table            : in String := "LNM$PROCESS") is

    Return_Status : Condition_Handling.Cond_Value_Type;

    procedure Lib_Set_Logical
      (Status : out Condition_Handling.Cond_Value_Type;
       Lognam : in  Starlet.Logical_Name_Type;
       Value  : in  String                    := "";
       Tabnam : in  Starlet.Logical_Name_Type := "LNM$PROCESS";
       Attr   : in  System.Unsigned_Longword  := System.Unsigned_Longword'Null_Parameter;
       Itmlst : in  Starlet.Item_List_Type    := Starlet.Item_List_Type'Null_Parameter);

    pragma Interface (Vaxrtl, Lib_Set_Logical);

    pragma Import_Valued_Procedure (Lib_Set_Logical, "LIB$SET_LOGICAL",
                                    Mechanism => (Value,
                                                  Descriptor(S),
                                                  Descriptor(S),
                                                  Descriptor(S),
                                                  Reference,
                                                  Reference ));

  begin
    Lib_Set_Logical(Status => Return_Status,
                    Lognam => Upper_Case(Logical_Name),
                    Value  => Upper_Case(Equivalence_Name),
                    Tabnam => Upper_Case(Table));
    if not Condition_Handling.Success(Return_Status) then
      raise Logical_Name_Error;
    end if;
  end Define;


  procedure Deassign (Logical_Name : in String;
                      Table        : in String := "LNM$PROCESS") is

    Return_Status : Condition_Handling.Cond_Value_Type;

    procedure Lib_Delete_Logical
      (Status : out Condition_Handling.Cond_Value_Type;
       Lognam : in  Starlet.Logical_Name_Type;
       Tabnam : in  Starlet.Logical_Name_Type := "LNM$PROCESS");

    pragma Interface (Vaxrtl, Lib_Delete_Logical);

    pragma Import_Valued_Procedure (Lib_Delete_Logical, "LIB$DELETE_LOGICAL",
                                    Mechanism => (Value,
                                                  Descriptor(S),
                                                  Descriptor(S) ));

  begin
    Lib_Delete_Logical(Status => Return_Status,
                       Lognam => Upper_Case(Logical_Name),
                       Tabnam => Upper_Case(Table));
    if Return_Status = Starlet.Ss_Nolognam then
      raise Logical_Name_Not_Found;
    elsif not Condition_Handling.Success(Return_Status) then
      raise Logical_Name_Error;
    end if;
  end Deassign;

end VMS_Logical_Names;
