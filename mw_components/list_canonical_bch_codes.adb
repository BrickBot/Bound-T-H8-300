-- PROCEDURE FOR LISTING CANONICAL REPRESENTATIONS OF BCH CODES
   ------------------------------------------------------------

-- Revision : 23-FEB-1987 by Mats Weber, modified the order in which the codes are listed.

-- Creation :  3-NOV-1986 by Mats Weber.


with Text_IO,
     Integer_Text_IO,
     Number_Images,
     User_Interface,
     Integer_Primes,
     ZpZ_Base_Type,
     Canonical_BCH_Codes;

use Text_IO,
    Integer_Text_IO,
    Integer_Primes,
    ZpZ_Base_Type,
    Canonical_BCH_Codes;

procedure List_Canonical_BCH_Codes is
----------------------------------

  N_Max         : Positive;
  T_Min, T_Max  : Positive;

  Last_P  : Natural := 0;
  Last_T  : Natural := 0;

  List_Uncheckable_Codes : Boolean;


  function Image is new Number_Images.Integer_Image(Integer);


  procedure Put_BCH_Code (P, N, T : in Positive; Comment : in String := "") is
  begin
    if Last_P /= 0 and Last_T /= 0 then
      if Last_T /= T then
        New_Line(2);
      elsif Last_P /= P then
        New_Line;
      end if;
    end if;
    Put("BCH(" & Image(P) & "," & Image(N) & "," & Image(T) & ")");
    if Comment /= "" then
      Put(" " & Comment);
    end if;
    New_Line;
    Last_P := P;
    Last_T := T;
  end Put_BCH_Code;


begin
  Put("Enter minimum and maximum values for T : ");
  Get(T_Min); Get(T_Max); Skip_Line;
  Put("Enter maximum value for N : ");
  Get(N_Max); Skip_Line;
  List_Uncheckable_Codes :=
    User_Interface.Yes_No_Answer("List codes that cannot be checked because p**n is too large ? ");
  for T in T_Min..T_Max loop
    for P in 1..T-1 loop
      if Prime(P) then
        for N in 1..N_Max loop
          begin
            if Canonical(ZpZ_Positive(P), N, T) then
              declare

                Div_N : constant Int_Array := Divisors(N, Include_1 => True);

                Found_Imbricated_Code : Boolean := False;

              begin
                M_Loop:
                  for I in Div_N'Range loop
                    declare

                      M : Positive renames Div_N(I);

                    begin
                      if Canonical(ZpZ_Integer(P), M, T) then
                        Found_Imbricated_Code := True;
                        exit M_Loop;
                      end if;
                    exception
                      when T_Out_Of_Range => null;
                    end;
                  end loop M_Loop;
                if not Found_Imbricated_Code then
                  Put_BCH_Code(P, N, T);
                end if;
              end;
            end if;
          exception
            when T_Out_Of_Range =>
              null;
            when Numeric_Error | Constraint_Error =>
              if List_Uncheckable_Codes then
                Put_BCH_Code(P, N, T, Comment => "cannot be checked, " & Image(P) & "**" & Image(N) & " is too large");
              end if;
          end;
        end loop;
      end if;
    end loop;
  end loop;
end List_Canonical_BCH_Codes;
