-- VMS LOGICAL NAMES
   -----------------

-- Creation :  2-OCT-1986 by Mats Weber.

package VMS_Logical_Names is
-------------------------

  function Translate (Logical_Name : String;
                      Table        : String  := "LNM$DCL_LOGICAL";
                      Iterative    : Boolean := True) return String;

  procedure Define (Logical_Name,
                    Equivalence_Name : in String;
                    Table            : in String := "LNM$PROCESS");

  procedure Deassign (Logical_Name : in String;
                      Table        : in String := "LNM$PROCESS");

  Logical_Name_Not_Found,
  Logical_Name_Error       : exception;

end VMS_Logical_Names;
