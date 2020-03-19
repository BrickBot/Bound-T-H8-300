-- GENERIC PACKAGE FOR STUDYING HYPERCUBES
   ---------------------------------------

-- Creation : 24-AUG-1989 by Mats Weber.


with Largest_Numeric_Types;

generic
   Dimension : in Natural;
package Hypercube is
-----------------

   -- Representation of a node by its coordinates

   type Node_Coordinate is range 0..1;

   type Node_Coordinate_Index is new Natural range 0..Dimension - 1;

   type Node_Coordinates is array (Node_Coordinate_Index) of Node_Coordinate;

   pragma Pack(Node_Coordinates);


   type Node_Array is array (Node_Coordinate_Index) of Node_Coordinates;

   function Neighbours (Of_Node : Node_Coordinates) return Node_Array;


   -- Representation of a node by a number

   use Largest_Numeric_Types;

   type Node_Number is new Largest_Numeric_Types.Large_Integer
      range 0..2 ** Dimension - 1;


   -- Conversion functions

   function Number_Of (Coordinates : Node_Coordinates) return Node_Number;

   function Coordinates_Of (Number : Node_Number) return Node_Coordinates;


   function Image (Of_Coordinates : Node_Coordinates) return String;
      -- Returns Of_Coordinates as a string of '0's and '1's


   generic
      with procedure Action (On_Node : in Node_Coordinates);
   procedure Hamiltonian_Circuit;

end Hypercube;
