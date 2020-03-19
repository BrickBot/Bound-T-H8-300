with Fast_Galois_Field,
     BCH_Code,
     Min_Max_Functions,
     Text_IO,
     Integer_Text_IO;

use Text_IO,
    Integer_Text_IO;

procedure Test_BCH_Code_Fast is

  function Get_Integer (Prompt : String) return Integer is

    N : Integer;

  begin
    Put(Prompt);
    Get(N);
    Skip_Line;
    return N;
  end;


  function Yes_No_Answer (Question : String) return Boolean is
    Ch : Character;
  begin
    loop
      Put(Question);
      loop
        Get(Ch);
        exit when Ch /= ' ';
      end loop;
      Skip_Line;
      exit when Ch = 'Y' or Ch = 'N' or Ch = 'y' or Ch = 'n';
    end loop;
    return Ch = 'Y' or Ch = 'y';
  end Yes_No_Answer;

begin
  begin
    Set_Line_Length(132);
  exception
    when Use_Error => null;  -- VAX Ada raises this exception if the file
  end;                       -- is associated with a terminal.
  declare

    P : constant Positive := Get_Integer("Enter p : ");
    N : constant Positive := Get_Integer("Enter n : ");
    T : constant Positive := Get_Integer("Enter t : ");

    -- P : constant := 2;
    -- N : constant := 8;

    package GF_P_N is new Fast_Galois_Field(P,N);

    use GF_P_N,
        GF_P_N.Base_ZpZ_Field,
        GF_P_N.Base_ZpZ_Field.ZpZ_Polynomials;

    package BCH_Code_P_N_T is new BCH_Code(P => P,
                                           N => N,
                                           Q => Q,
                                           T => T,
                                           ZpZ => ZpZ,
                                           Element => Element,
                                           Zero => Zero,
                                           One  => One,
                                           Generator => Generator,
                                           Polynomial => Polynomial,
                                           Zero_Polynomial => Zero_Polynomial);

    use BCH_Code_P_N_T;

    package ZpZ_Text_IO is new Integer_IO(ZpZ);
    use ZpZ_Text_IO;


    function Get_Polynomial (Prompt : String) return Polynomial is

      Deg : Degree;

    begin
      Put_Line(Prompt);
      Put("Degree : "); Get(Deg); Skip_Line;
      declare
        P : Polynomial(0..Deg);
      begin
        for I in reverse P'Range loop
          Put("Coefficient of x^"); Put(I, Width => 0); Put(" : ");
          Get(P(I)); Skip_Line;
        end loop;
        return P;
      end;
    end Get_Polynomial;


    procedure Put (P : in Polynomial) is

      Deg_P : constant Degree := Deg(P);

    begin
      if Deg_P = -1 then
        Put("0");
      else
        for I in reverse P'Range loop
          if P(I) /= 0 then
            if P(I) >= 0 then
              if I /= Deg_P then
                Put(" + ");
              end if;
            else
              if I = Deg_P then
                Put("- ");
              else
                Put(" - ");
              end if;
            end if;
            if abs P(I) /= 1 or I = 0 then
              Put(abs P(I), Width => 0);
            end if;
            case I is
              when 0 =>
                null;
              when 1 =>
                Put("x");
              when others =>
                Put("x^"); Put(I, Width => 0);
            end case;
          end if;
        end loop;
      end if;
    end Put;


    procedure Put_Line (P : in Polynomial) is
    begin
      Put(P);
      New_Line;
    end;

  begin
    Put("This representation is ");
    if not Canonical_Representation then
      Put("not ");
    end if;
    Put("canonical");
    New_Line;
    Put("The canonical representation for this code is BCH(");
    Put(P, Width => 0); Put(',');
    Put(N, Width => 0); Put(',');
    Put(BCH_Bound, Width => 0); Put(").");
    New_Line;
    Put("Generator polynomial g(x) : ");
    Put(Generator_Polynomial);
    New_Line;
    Put("Weight of the generator polynomial : ");
    Put(Weight(To_Word(Generator_Polynomial)), Width => 0);
    New_Line;
    Put("Associated polynomial h(x) : ");
    Put(Associated_Polynomial);
    New_Line;
    Put("g(x) * h(x) : ");
    Put(Generator_Polynomial * Associated_Polynomial);
    New_Line;
    Put("Dimension of the code : ");
    Put(Dimension, Width => 0);
    New_Line;
    while Yes_No_Answer("Display weight distribution ? (y/n) ") loop
      New_Line;
      declare

        Max_Bar_Length : constant := 100;

        type Natural_Array is array (Hamming_Weight range <>) of Natural;

        function Max is
           new Min_Max_Functions.Array_Maximum(Index => Hamming_Weight,
                                               Item  => Natural,
                                               Row   => Natural_Array);

        Frequencies   : Distribution;
        Max_Frequency : Natural;
        Coefficient   : Float;

        Exact : constant Boolean := Yes_No_Answer("Exact distribution ? (y/n) ");

      begin
        if Exact then
          Frequencies := Weight_Distribution;
        else
          declare
            N : Natural;
          begin
            Put("How many random words ? ");
            Get(N);
            Skip_Line;
            Frequencies := Approximative_Weight_Distribution(N);
          end;
        end if;
        Put_Line("Weight distribution : ");
        New_Line;
        Max_Frequency := Max(Natural_Array(Frequencies));
        if Max_Frequency <= Max_Bar_Length then
          Coefficient := 1.0;
        else
          Coefficient := Float(Max_Bar_Length)/Float(Max_Frequency);
        end if;
        for I in Frequencies'Range loop
          Put(I, Width => 5);
          Put(Frequencies(I), Width => 8);
          Put("   ");
          for J in 1..Integer(Coefficient * Float(Frequencies(I))) loop
            Put('*');
          end loop;
          New_Line;
        end loop;
        New_Line;
        if Exact then
          Put("HAMMING NUMBER : ");
          Put(Hamming_Number, Width => 0);
          New_Line;
          Put("Errors corrected : ");
          Put(Errors_Corrected, Width => 0);
          New_Line;
          Put("Errors detected : ");
          Put(Errors_Detected, Width => 0);
          New_Line;
        end if;
      end;
    end loop;
  end;
end Test_BCH_Code_Fast;
