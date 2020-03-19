--+ TITLE:      Test of DISCRETE_SETS package.
--+ SUPPORT:    N.H. LAM,  CGL-DMA-EPFL,  CH-1015 LAUSANNE
--+ CREATION:   31-JUL-1985 by NHL.

with Discrete_Sets;
with Text_IO; use Text_IO;

procedure Test_Discrete_Sets is
----------------------------

  --+ OVERVIEW: Test of DISCRETE_SETS package.
  --+ ERRORS:   Raise TEST_ERROR exception when a test does not complete


  type Color is (Blanc, Bleu, Jaune, Vert, Rouge, Orange, Noir);
  package Color_Set is new Discrete_Sets (Color, Count => Natural);
  use Color_Set ;

  France: constant Set:=New_Set ((Blanc,Bleu,Rouge));
  Allemagne: constant Set := New_Set ((Noir,Rouge,Jaune));
  Suisse: constant Set := New_Set ((Blanc,Rouge));

  Test_Error: exception;


  procedure Test_Membership is
  begin
    if Member(Noir,Suisse) or  not Member(Noir,Allemagne) then raise Test_Error; end if;
  end Test_Membership;


  procedure Test_Difference is
  begin
    if Suisse - Allemagne /= New_Set (Blanc) then raise Test_Error; end if;
    if France - (Blanc,Rouge,Noir) /= New_Set (Bleu) then
      raise Test_Error;
    end if;
    if (Blanc, Noir, Rouge, Vert) - Suisse  /=  New_Set (Vert) + Noir  then
      raise Test_Error;
    end if;
    if France - Noir /= France then raise Test_Error; end if;
    if -Suisse /= Full_Set - Suisse then raise Test_Error; end if;
  end Test_Difference;


  procedure Test_Minmax is
  begin
    if Max(Allemagne) /= Noir  or  Min(Allemagne) /= Jaune then
      raise Test_Error;
    end if;
  end Test_Minmax;


  procedure Test_Full_Empty is
  begin
    if Full(Suisse) or not Full(New_Set((Blanc,Bleu,Jaune,Vert,Rouge,Orange,Noir))) then
      raise Test_Error;
    end if;
    if Empty(Suisse) or Empty(New_Set((Blanc,Bleu,Jaune,Vert,Rouge,Orange,Noir))) then
      raise Test_Error;
    end if;
  end Test_Full_Empty;


  procedure Test_Union is
  begin
    if Suisse + Allemagne /= New_Set ((Rouge, Blanc, Rouge, Jaune, Noir)) then
      raise Test_Error;
    end if;
    if France + (Blanc,Rouge,Noir) /= New_Set ((Noir, Bleu, Blanc, Rouge)) then
      raise Test_Error;
    end if;
    if (Blanc, Rouge, Noir, Vert) + Suisse /=
                                      New_Set ((Blanc, Rouge, Noir, Vert)) then
      raise Test_Error;
    end if;
  end Test_Union;


  procedure Test_Intersection is
  begin
    if Suisse * Allemagne /= New_Set (Rouge) then
      raise Test_Error;
    end if;
    if France * (Blanc,Rouge,Noir) /= New_Set ((Rouge, Blanc)) then
      raise Test_Error;
    end if;
    if (Blanc, Rouge, Noir, Vert) * Suisse /= Suisse  then
      raise Test_Error;
    end if;
  end Test_Intersection;


  procedure Test_Forall_Enumeration is
    -- Build FULL_LIST from FULL_SET, then check for each element of
    --   FULL_SET to be in FULL_LIST.

    Full_List: List (Color'Pos(Color'First)+1..Color'Pos(Color'Last)+1)
       := To_List (Full_Set);

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

  begin  -- TEST_FORALL_ENUMERATION
    Check (Full_Set);
  end Test_Forall_Enumeration;


begin  --  TEST_DISCRETE_SETS
  Test_Membership;
  Test_Difference;
  Test_Minmax;
  Test_Full_Empty;
  Test_Union;
  Test_Intersection;
  Test_Forall_Enumeration;
end Test_Discrete_Sets;
