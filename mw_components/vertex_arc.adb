-- DEFINITION OF TYPES VERTEX AND ARC FOR GRAPH_HANDLER
   ----------------------------------------------------

-- Creation : 14-NOV-1985 by Mats Weber


package body Vertex_Arc is
-----------------------

  function New_Vertex return Vertex is
  begin
    return New_Key;
  exception
    when Vertex_Keys.Too_Many_Keys =>
      raise Too_Many_Vertices;
  end New_Vertex;


  function Smaller (V1, V2 : Vertex) return Boolean is
  begin
    return V1 < V2;
  exception
    when Vertex_Keys.Uninitialized_Key =>
      raise Uninitialized_Vertex;
  end Smaller;


  procedure Check (The_Vertex : in Vertex) is
  begin
    if not Is_Initialized(The_Vertex) then
      raise Uninitialized_Vertex;
    end if;
  end Check;


  function New_Arc (Initial, Final : Vertex) return Arc is
  begin
    return Arc'(Signature => Arc_Keys.New_Key,
                Initial   => Initial,
                Final     => Final);
  exception
    when Arc_Keys.Too_Many_Keys =>
      raise Too_Many_Arcs;
  end New_Arc;


  function Initial (Of_Arc : Arc) return Vertex is
  begin
    Check(The_Arc => Of_Arc);
    return Of_Arc.Initial;
  end Initial;


  function Final (Of_Arc : Arc) return Vertex is
  begin
    Check(The_Arc => Of_Arc);
    return Of_Arc.Final;
  end Final;


  function Other_End (Of_Arc : Arc; This_End : Vertex) return Vertex is
  begin
    Check(The_Arc => Of_Arc);
    if Of_Arc.Initial = This_End then
      return Of_Arc.Final;
    elsif Of_Arc.Final = This_End then
      return Of_Arc.Initial;
    else
      raise Vertex_Not_On_Arc;
    end if;
  end Other_End;


  function Smaller (A1, A2 : Arc) return Boolean is
  begin
    Check(The_Arc => A1);
    Check(The_Arc => A2);
    if A1.Initial /= A2.Initial then
      return Smaller(A1.Initial, A2.Initial);
    elsif A1.Final /= A2.Final then
      return Smaller(A1.Final, A2.Final);
    else
      return Arc_Keys."<"(A1.Signature, A2.Signature);
    end if;
  end Smaller;


  procedure Check (The_Arc : in Arc) is
  begin
    if not Arc_Keys.Is_Initialized(The_Arc.Signature) then
      raise Uninitialized_Arc;
    end if;
  end Check;

end Vertex_Arc;
