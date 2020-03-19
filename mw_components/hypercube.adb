-- GENERIC PACKAGE FOR STUDYING HYPERCUBES
   ---------------------------------------

-- Creation : 24-AUG-1989 by Mats Weber.


package body Hypercube is
----------------------

   function Neighbours (Of_Node : Node_Coordinates) return Node_Array is

      Result : Node_Array := (others => Of_Node);

   begin
      for I in Node_Coordinate_Index loop
         Result(I)(I) := 1 - Of_Node(I);
      end loop;
      return Result;
   end Neighbours;


   function Number_Of (Coordinates : Node_Coordinates) return Node_Number is

      Result : Node_Number := 0;

   begin
      for I in reverse Coordinates'Range loop
         Result := 2 * Result + Node_Number(Coordinates(I));
      end loop;
      return Result;
   end Number_Of;


   function Coordinates_Of (Number : Node_Number) return Node_Coordinates is

      Result     : Node_Coordinates;
      Remainder  : Node_Number := Number;

   begin
      for I in Result'Range loop
         Result(I) := Node_Coordinate(Remainder mod 2);
         Remainder := Remainder / 2;
      end loop;
      return Result;
   end Coordinates_Of;


   function Image (Of_Coordinates : Node_Coordinates) return String is

      Result : String(1..Of_Coordinates'Length);

   begin
      for I in Of_Coordinates'Range loop
         case Of_Coordinates(I) is
            when 0 =>
               Result(Natural(I) + 1) := '0';
            when 1 =>
               Result(Natural(I) + 1) := '1';
         end case;
      end loop;
      return Result;
   end Image;


   procedure Hamiltonian_Circuit is

      V : Node_Coordinates;

      type Direction is (Forward, Backward);

      First   : constant array (Direction) of Node_Coordinate := (Forward => 0, Backward => 1);
      Second  : constant array (Direction) of Node_Coordinate := (Forward => 1, Backward => 0);


      procedure Hamilton (N : in Node_Coordinate_Index; The_Direction : in Direction) is

         procedure Next_Dimension (The_Direction : in Direction) is
         begin
            if N = 0 then
               Action(V);
            else
               Hamilton(N - 1, The_Direction);
            end if;
         end Next_Dimension;

      begin
         V(N) := First(The_Direction);
         Next_Dimension(The_Direction => Forward);
         V(N) := Second(The_Direction);
         Next_Dimension(The_Direction => Backward);
      end Hamilton;

   begin
      Hamilton(Node_Coordinate_Index'Last, The_Direction => Forward);
   end Hamiltonian_Circuit;

end Hypercube;
