--+ TITLE:      Test of GENERAL_SETS package.
--+ SUPPORT:    N.H. LAM,  CGL-DMA-EPFL,  CH-1015 LAUSANNE
--+ CREATION:   24-JUL-1985 by NHL.

with General_Sets;
with Text_IO; use Text_IO;

procedure Test_General_Sets is
---------------------------

  --+ OVERVIEW: Test of GENERAL_SETS package.
  --+ ERRORS:   Raise TEST_ERROR exception when a test does not complete

  generic
     type Element_Type is private;
     type Set is limited private;
     type Positive_Count is range <>;
     type List is array (Positive_Count range <>) of Element_Type;
     with procedure Add (Element  : in Element_Type; To : in out Set) is <>;
     with procedure Add (Elements : in Set;          To : in out Set) is <>;
     with procedure Remove (Elements : in Set;          From : in out Set) is <>;
     with procedure Intersect (Left : in Set;  Right : in Set;  To : in out Set) is <>;
  package Additional_Set_Operations is

     function New_Set (Items : in List) return Set;
     function New_Set (Item : in Element_Type) return Set;

     function "+" (Left, Right : Set) return Set;
     function "*" (Left, Right : Set) return Set;
     function "-" (Left, Right : Set) return Set;

  end Additional_Set_Operations;

  type Color is (Blanc, Bleu, Jaune, Vert, Rouge, Orange, Noir);
  package Color_Set is new General_Sets(Color, Count => Natural);
  use Color_Set;

  France, Allemagne, Suisse: Set;

  Test_Error: exception;


  package body Additional_Set_Operations is

     function New_Set (Items : in List) return Set is

        Result : Set;

     begin
        for I in Items'Range loop
           Add(Items(I), To => Result);
        end loop;
        return Result;
     end New_Set;

     function New_Set (Item : in Element_Type) return Set is

        Result : Set;

     begin
        Add(Item, To => Result);
        return Result;
     end New_Set;



     function "+" (Left, Right : Set) return Set is

        Result : Set;

     begin
        Add(Left, To => Result);
        Add(Right, To => Result);
        return Result;
     end "+";


     function "*" (Left, Right : Set) return Set is

        Result : Set;

     begin
        Intersect(Left, Right, To => Result);
        return Result;
     end "*";


     function "-" (Left, Right : Set) return Set is

        Result : Set;

     begin
        Add(Left, To => Result);
        Remove(Right, From => Result);
        return Result;
     end "-";

  end Additional_Set_Operations;


  package Color_Set_Plus is new Additional_Set_Operations(Color,
                                                          Color_Set.Set,
                                                          Color_Set.Positive_Count,
                                                          Color_Set.List);
  use Color_Set_Plus;


  procedure Put (Element:Color) is
    package My_Enum_IO is new Text_IO.Enumeration_IO(Color);
  begin
    My_Enum_IO.Put(Element);
    Text_IO.Put(' ');
  end Put;

  procedure Print_Set is new Color_Set.Enumeration(Put);

  procedure Test_Membership  is
  begin
    if Member(Noir,Suisse) then raise Test_Error; end if;
    if not Member(Blanc,New_Set ((Blanc, Bleu, Rouge, Noir))) then
      raise Test_Error;
    end if;
  end Test_Membership;


  procedure Test_Difference is
  begin
    if Suisse - Allemagne /= New_Set (Blanc) then
      raise Test_Error;
    end if;
    if France - New_Set((Blanc, Rouge, Noir)) /= New_Set (Bleu) then
      raise Test_Error;
    end if;
    if New_Set((Blanc, Rouge, Noir, Vert)) - Suisse /=
                      New_Set ((Noir, Vert)) then
      raise Test_Error;
    end if;
    if France - New_Set(Rouge) /= New_Set ((Bleu, Blanc)) then
      raise Test_Error;
    end if;
  end Test_Difference;


  procedure Test_Inclusion is
  begin
    if Suisse < Allemagne then raise Test_Error; end if;
    if not (France < (Blanc, Bleu, Rouge, Noir)) then raise Test_Error; end if;
    if (Blanc,Rouge,Noir,Vert) < Suisse then raise Test_Error; end if;
    if Suisse <= Allemagne then raise Test_Error; end if;
    if not (France <= (Blanc,Bleu,Rouge,Noir)) then raise Test_Error; end if;
    if (Blanc,Rouge,Noir,Vert) <= Suisse then raise Test_Error; end if;
    if (Blanc,Rouge) < Suisse then raise Test_Error; end if;
    if not ((Blanc,Rouge) <= Suisse) then raise Test_Error; end if;
  end Test_Inclusion;


  procedure Test_Intersection is
  begin
    if Suisse * Allemagne /= New_Set (Rouge) then
      raise Test_Error;
    end if;
    if France * New_Set((Blanc,Rouge,Noir)) /= New_Set ((Blanc, Rouge)) then
      raise Test_Error;
    end if;
    if New_Set((Noir,Bleu)) * New_Set((Orange, Vert, Rouge, Blanc, Noir)) /=
                                            New_Set (Noir) then
      raise Test_Error;
    end if;
    if New_Set((Blanc,Rouge,Noir,Vert)) * Suisse /= Suisse then
      raise Test_Error;
    end if;
  end Test_Intersection;


  procedure Test_Min_Max  is
  begin
    if Min(Allemagne) /= Jaune then raise Test_Error; end if;
    if Max(Allemagne) /= Noir then raise Test_Error; end if;
  end Test_Min_Max;


  procedure Test_Empty is
    T: Set;
  begin
    if Empty(Suisse) then raise Test_Error; end if;
    if not Empty(T) then raise Test_Error; end if;
  end Test_Empty;


  procedure Test_Union   is
  begin
    if Suisse + Allemagne /= New_Set ((Blanc, Rouge, Jaune, Noir)) then
      raise Test_Error;
    end if;
    if France + New_Set((Blanc,Rouge,Noir)) /= New_Set ((Blanc,Bleu,Rouge,Noir)) then
      raise Test_Error;
    end if;
    if New_Set((Blanc, Noir, Vert)) + Suisse
                  /= New_Set ((Blanc, Rouge, Noir, Vert)) then
      raise Test_Error;
    end if;
    if (Suisse + New_Set(Bleu) /= France) or (New_Set(Bleu) + Suisse /= France) then
      raise Test_Error;
    end if;
    if (Suisse + New_Set(Rouge) /= Suisse) or (New_Set(Rouge) + Suisse /= Suisse) then
      raise Test_Error;
    end if;
  end Test_Union;


  procedure Test_Forall_Enumeration is
    -- Build FULL_LIST from FULL_SET, then check for each element of
    --   FULL_SET to be in FULL_LIST.

    Full_Set: Set;
    Full_List: List (Color'Pos(Color'First)+1..Color'Pos(Color'Last)+1);

    procedure Check_In_Full_List (Fcolor: Color) is
      -- Search for FCOLOR in FULL_LIST.
      -- Raise exception TEST_ERROR if FCOLOR is not found.
      I: Natural := Full_List'First;
    begin  -- CHECK_IN_FULL_LIST
      while I <= Full_List'Last and then Fcolor /= Full_List(I) loop
        I := I+1;
      end loop;
      if I > Full_List'Last then
        raise Test_Error;  -- FCOLOR was not found
      end if;
    end Check_In_Full_List;

    procedure Check is new Color_Set.Enumeration(Check_In_Full_List);

  begin  -- Test_Forall_Enumeration
    Assign(Full_Set,New_Set ((Blanc, Bleu, Jaune, Vert, Rouge, Orange, Noir)));
    Full_List := Color_Set.To_List (Full_Set);
    Check (Full_Set);
  end Test_Forall_Enumeration;


  procedure Test_Float is
    package Genset_Of_Float is new General_Sets(Float, Count => Natural);
    use Genset_Of_Float;

    package Genset_Of_Float_Plus is new Additional_Set_Operations(Float,
                                                                  Genset_Of_Float.Set,
                                                                  Genset_Of_Float.Positive_Count,
                                                                  Genset_Of_Float.List);
    use Genset_Of_Float_Plus;

    S1, S2, S3: Genset_Of_Float.Set;
  begin
    Genset_Of_Float.Assign(S1, New_Set((4.0,7.0,3.0,10.0)));
    Genset_Of_Float.Assign(S2, New_Set((10.0,5.0,7.0,15.0)));

    Genset_Of_Float.Assign(S3, S1 + S2);
    if not (S2 <= S3  and  S1 <= S3) then
      raise Test_Error;
    end if;
    if not (S3 * S1 = S1  and  S2 * S3 = S2) then
      raise Test_Error;
    end if;

    Assign(S2,S1);
    if not (S2 <= S1  and  S1 <= S2) then
      raise Test_Error;
    end if;
  end Test_Float;


begin  -- TEST_GENERAL_SETS

  Color_Set.Assign ( Suisse,New_Set ((Rouge, Blanc)));
  Color_Set.Assign ( France,New_Set ((Blanc, Bleu, Rouge)));
  Color_Set.Assign ( Allemagne,New_Set ((Noir, Rouge, Jaune)));

  Test_Membership;
  Test_Difference;
  Test_Inclusion ;
  Test_Intersection;
  Test_Min_Max;
  Test_Empty;
  Test_Union;
  Test_Forall_Enumeration;
  Test_Float;
end Test_General_Sets;
