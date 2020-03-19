with Hypercube,
     Discrete_Sets,
     Number_Images,
     Text_IO,
     User_Interface;

use Text_IO;

procedure Hamiltonian_Circuits_On_Hypercube is
-------------------------------------------

   function Positive_Answer is new User_Interface.Integer_Answer(Positive);

   Dimension : constant Positive := Positive_Answer("Dimension : ");

   package The_Hypercube is new Hypercube(Dimension);
   use The_Hypercube;


   type Node_List is array (Natural range <>) of Node_Coordinates;


   Counter : Natural := 0;


   generic
      with procedure Action (Nodes : in Node_List);
   procedure Enumerate_Hamiltonian_Circuits;

   procedure Enumerate_Hamiltonian_Circuits is

      subtype Path_Length is Natural range 0..2 ** Dimension;
      subtype Path_Depth  is Natural range 0..Path_Length'Last - 1;

      package Node_Sets is new Discrete_Sets(Node_Number, Count => Path_Length);
      use Node_Sets;

      Path : Node_List(Path_Depth);
      Mark : Node_Sets.Set := Empty_Set;


      procedure Find_Path (From : in Node_Coordinates; Depth : in Path_Length) is

         Number_Of_From  : constant Node_Number := Number_Of(From);
         Next            : constant Node_Array := Neighbours(Of_Node => From);

      begin
         if Member(Number_Of_From, Of_Set => Mark) then
            if Depth > Path'Last and From = (Node_Coordinate_Index => 0) then
               Action(Path);
            end if;
         else
            Add(Number_Of_From, To => Mark);
            Path(Depth) := From;
            for I in Next'Range loop
               Find_Path(From => Next(I), Depth => Depth + 1);
            end loop;
            Remove(Number_Of_From, From => Mark);
         end if;
      end Find_Path;

   begin
      Find_Path(From => (Node_Coordinate_Index => 0), Depth => 0);
   end Enumerate_Hamiltonian_Circuits;


   procedure Put_Hamiltonian_Circuit (Circuit : in Node_List) is
   begin
      for I in Circuit'Range loop
         Put(" " & Image(Circuit(I)));
      end loop;
      New_Line;
   end Put_Hamiltonian_Circuit;


   procedure Put_Counted_Hamiltonian_Circuit (Circuit : in Node_List) is

      function Image is new Number_Images.Integer_Image(Positive,
                                                        Default_Width => 4);

   begin
      Counter := Counter + 1;
      Put(Image(Counter) & ")");
      Put_Hamiltonian_Circuit(Circuit);
   end Put_Counted_Hamiltonian_Circuit;


   procedure Put_Hamiltonian_Circuits is
      new Enumerate_Hamiltonian_Circuits(Put_Counted_Hamiltonian_Circuit);


   function Canonical_Hamiltonian_Circuit return Node_List is

      Result  : Node_List(0..2 ** Dimension - 1);
      Index   : Natural range Result'First..Result'Last + 1 := Result'First;

      procedure Put_Into_Result (Node : in Node_Coordinates) is
      begin
         Result(Index) := Node;
         Index := Index + 1;
      end Put_Into_Result;

      procedure Build_Hamiltonian_Circuit is
         new Hamiltonian_Circuit(Put_Into_Result);

   begin
      Build_Hamiltonian_Circuit;
      return Result;
   end Canonical_Hamiltonian_Circuit;

begin
   Put_Line("Hamiltonian circuit given by package Hypercube :");
   Put_Hamiltonian_Circuit(Canonical_Hamiltonian_Circuit);
   New_Line;
   if User_Interface.Yes_No_Answer("Generate all hamiltonian circuits ? ") then
      Put_Hamiltonian_Circuits;
   end if;
end Hamiltonian_Circuits_On_Hypercube;
