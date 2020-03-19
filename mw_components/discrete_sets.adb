-- GENERIC PACKAGE FOR SETS OF DISCRETE TYPES
   ------------------------------------------

-- Creation : 20-JUN-1985 by S. Mourtada.


with Exchange;

package body Discrete_Sets is
--------------------------

  function New_Set (Element : Element_Type) return Set is

    S : Set := Empty_Set;

  begin
    S(Element) := True;
    return S;
  end New_Set;


  function New_Set (Elements : List) return Set is

    S : Set := Empty_Set;

  begin
    for I in Elements'Range loop
      S(Elements(I)) := True;
    end loop;
    return S;
  end New_Set;


  function "-" (Right : Set) return Set is
  begin
    return Full_Set - Right;
  end "-";


  function "+" (Left : Set; Right : Set) return Set is
  begin
    return Left or Right;
  end "+";

  function "+" (Left : Set; Right : List) return Set is
  begin
    return Left + New_Set(Right);
  end "+";

  function "+" (Left : List; Right : Set) return Set is
  begin
    return New_Set(Left) + Right;
  end "+";

  function "+" (Left : Set; Right : Element_Type) return Set is
  begin
    return Left + New_Set(Right);
  end "+";

  function "+" (Left : Element_Type; Right : Set) return Set is
  begin
    return New_Set(Left) + Right;
  end "+";


  function "*" (Left : Set; Right : Set) return Set is
  begin
    return Left and Right;
  end "*";

  function "*" (Left : Set; Right : List) return Set is
  begin
    return New_Set(Right) * Left;
  end "*";

  function "*" (Left : List; Right : Set) return Set is
  begin
    return Right * New_Set(Left);
  end "*";


  function "-" (Left : Set; Right : Set) return Set is
  begin
    return Left and not Right;
  end "-";

  function "-" (Left : Set; Right : List) return Set is
  begin
    return Left - New_Set(Right);
  end "-";

  function "-" (Left : List; Right : Set) return Set is
  begin
    return New_Set(Left) - Right;
  end "-";

  function "-" (Left : Set; Right : Element_Type) return Set is
  begin
    return  Left - New_Set(Right);
  end "-";


  function "<" (Left : Set; Right : Set) return Boolean is
  begin
    return ((Left and Right) = Left) and (Left /= Right);
  end "<";

  function "<" (Left : Set; Right : List) return Boolean is
  begin
    return Left < New_Set(Right);
  end "<";

  function "<" (Left : List; Right : Set) return Boolean is
  begin
    return New_Set(Left) < Right;
  end "<";


  function ">" (Left : Set; Right : Set) return Boolean is
  begin
    return ((Left and Right) = Right) and (Left /= Right);
  end ">";

  function ">" (Left : Set; Right : List) return Boolean is
  begin
    return Left > New_Set(Right);
  end ">";

  function ">" (Left : List; Right : Set) return Boolean is
  begin
    return New_Set(Left) > Right;
  end ">";


  function "<=" (Left : Set; Right : Set) return Boolean is
  begin
    return (Left and Right) = Left;
  end "<=";

  function "<=" (Left : Set; Right : List) return Boolean is
  begin
    return Left <= New_Set(Right);
  end "<=";

  function "<=" (Left : List; Right : Set) return Boolean is
  begin
    return New_Set(Left) <= Right;
  end "<=";


  function ">=" (Left : Set; Right : Set) return Boolean is
  begin
    return (Left and Right) = Right;
  end ">=";

  function ">=" (Left : Set; Right : List) return Boolean is
  begin
    return Left >= New_Set(Right);
  end ">=";

  function ">=" (Left : List; Right : Set) return Boolean is
  begin
    return New_Set(Left) >= Right;
  end ">=";


  function Member (Element : Element_Type; Of_Set : Set) return Boolean is
  begin
    return Of_Set(Element);
  end Member;

  function Member (Element : Element_Type; Of_Set : List) return Boolean is
  begin
    for I in Of_Set'Range loop
      if Element = Of_Set(I) then
        return True;
      end if;
    end loop;
    return False;
  end Member;


  function Max (Of_Set : Set) return Element_Type is
  begin
    for E in reverse Set'Range loop
      if Of_Set(E) then
        return E;
      end if;
    end loop;
    raise Set_Empty;
  end Max;

  function Min (Of_Set : Set) return Element_Type is
  begin
    for E in Set'Range loop
      if Of_Set(E) then
        return E;
      end if;
    end loop;
    raise Set_Empty;
  end Min;


  function Card (Of_Set : Set) return Natural_Count is

    N : Natural_Count := 0;

  begin
    for E in Set'Range loop
      if Of_Set(E) then
        N := N + 1;
      end if;
    end loop;
    return N;
  end Card;

  function Card (Of_List : List) return Natural_Count is
  begin
    return Card(New_Set(Of_List));
  end Card;


  procedure Enumeration (On_Set : in Set) is
  begin
    for E in Set'Range loop
      if On_Set(E) then
        Action(E);
      end if;
    end loop;
  end Enumeration;


  function To_List (From : Set) return List is

    L : List(1..Set'Length);
    I : Natural_Count := 0;

  begin
    for Elem in Set'Range loop
      if From(Elem) then
        I := I + 1;
        L(I) := Elem;
      end if;
    end loop;
    return L(1..I);
  end To_List;


  procedure Add (Element : in Element_Type; To : in out Set) is
  begin
    To(Element) := True;
  end Add;

  procedure Add (Elements : in Set; To : in out Set) is
  begin
    To := Elements or To;
  end Add;

  procedure Add (Elements : in List; To : in out Set) is
  begin
    for I in Elements'Range loop
      To(Elements(I)) := True;
    end loop;
  end Add;


  procedure Remove (Element : in Element_Type; From : in out Set) is
  begin
    From(Element) := False;
  end Remove;

  procedure Remove (Elements : in Set; From : in out Set) is
  begin
    From := From and not Elements;
  end Remove;

  procedure Remove (Elements : in List; From : in out Set) is
  begin
    for I in Elements'Range loop
      From(Elements(I)) := False;
    end loop;
  end Remove;


  procedure Intersect (Left : in Set; Right : in Set; To : out Set) is
  begin
    To := Left * Right;
  end Intersect;


  procedure Intersect (Left : in Set; Right : in List; To : out Set) is
  begin
    To := Left * Right;
  end Intersect;


  procedure Intersect (Left : in List; Right : in Set; To : out Set) is
  begin
    To := Left * Right;
  end Intersect;


  procedure Swap (Left, Right : in out Set) is

    procedure Swap is new Exchange(Set);

  begin
    Swap(Left, Right);
  end Swap;


  function Full (The_Set : Set) return Boolean is
  begin
    return The_Set = Full_Set;
  end Full;

  function Empty (The_Set : Set) return Boolean is
  begin
    return The_Set = Empty_Set;
  end Empty;


  procedure Empty (The_Set : out Set) is
  begin
    The_Set := Empty_Set;
  end Empty;

end Discrete_Sets;
