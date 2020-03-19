with Text_IO,
     Integer_Text_IO,
     Float_Text_IO,
     Character_Handler,
     String_Handler,
     Number_Images,
     User_Interface;

use Text_IO,
    Integer_Text_IO,
    Float_Text_IO,
    Character_Handler,
    String_Handler,
    Number_Images,
    User_Interface;

procedure Test_String_Handler is

  type Fix is delta 0.01 range -1000.0..1000.0;

  package Fix_Text_IO is new Text_IO.Fixed_IO(Fix);
  use Fix_Text_IO;

begin
  Integer_Text_IO.Default_Width := 0;
  --
  Put_Line("Test function locate");
  loop
    declare

      What  : constant String := String_Answer("Text : ");
      Frag  : constant String := String_Answer("Fragment : ");
      Ch    : Character;
      Loc   : Natural;

    begin
      loop
        Put("From the Right or the Left ? ");
        Get(Ch);
        Skip_Line;
        Upper_Case(Ch);
        exit when Ch = 'R' or Ch = 'L';
      end loop;
      case Ch is
        when 'R' =>
          Loc := Locate(Frag, What, Right);
        when 'L' =>
          Loc := Locate(Frag, What, Left);
        when others =>
          raise Program_Error;
      end case;
      Put("Location of the fragment : "); Put(Loc);
      New_Line;
      New_Line;
    end;
    exit when not Yes_No_Answer("MORE ? ");
  end loop;
  --
  Put_Line("Test function ""-""");
  loop
    declare

      What  : constant String := String_Answer("Text : ");
      Frag  : constant String := String_Answer("Fragment : ");

    begin
      Put_Line('"' & What & """ - """ & Frag & """ = """ & (What - Frag) & '"');
      if Frag'Length > 0 then
        Put_Line('"' & What & """ - '" & Frag(Frag'First) & "' = """ & (What - Frag(Frag'First)) & '"');
      end if;
      New_Line;
    end;
    exit when not Yes_No_Answer("MORE ? ");
  end loop;
  --
  declare

    X : Float;
    Y : Fix;
    N : Integer;

    Width, Fore, Aft, Exp : Natural;

    function Image is new Integer_Image(Integer);

    function Image is new Float_Image(Float);

    function Image is new Fixed_Image(Fix);

  begin
    Put_Line("Test Image functions");
    loop
      New_Line;
      Put("ENTER INTEGER VALUE : ");
      Get(N);
      Skip_Line;
      Put("  WIDTH => "); Get(Width); Skip_Line;
      Put_Line("  DEFAULT :   """ & Image(N) & """");
      Put_Line("  PARAMIZED : """ & Image(N, Width) & """");
      New_Line;
      Put("ENTER FLOAT VALUE : ");
      Get(X);
      Skip_Line;
      Put("  FORE => "); Get(Fore); Skip_Line;
      Put("  AFT  => "); Get(Aft);  Skip_Line;
      Put("  EXP  => "); Get(Exp);  Skip_Line;
      Put_Line("  DEFAULT :   """ & Image(X) & """");
      Put_Line("  PARAMIZED : """ & Image(X, Fore, Aft, Exp) & """");
      New_Line;
      Put("ENTER FIX VALUE : ");
      Get(Y);
      Skip_Line;
      Put("  FORE => "); Get(Fore); Skip_Line;
      Put("  AFT  => "); Get(Aft);  Skip_Line;
      Put("  EXP  => "); Get(Exp);  Skip_Line;
      Put_Line("  DEFAULT :   """ & Image(Y) & """");
      Put_Line("  PARAMIZED : """ & Image(Y, Fore, Aft, Exp) & """");
      New_Line;
      exit when not Yes_No_Answer("MORE ? ");
    end loop;
  end;
end Test_String_Handler;
