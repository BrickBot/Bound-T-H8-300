-- PACKAGE CONTAINING USEFUL DECLARATIONS FOR EXPLORING BCH CODES
   --------------------------------------------------------------

-- Creation : 18-AUG-1989 by Mats Weber.


with ZpZ_Base_Type,
     Fast_Galois_Field,
     BCH_Code,
     Matrices;

generic
   P : in ZpZ_Base_Type.ZpZ_Positive;
   N : in Positive;
   T : in Positive;
package Check_BCH_Bound_Common is
------------------------------

   package GF_P_N is new Fast_Galois_Field(P, N);

   use GF_P_N,
       GF_P_N.Base_ZpZ_Field,
       GF_P_N.Base_ZpZ_Field.ZpZ_Polynomials;


   package BCH_Code_P_N_T is new BCH_Code(P => P,
                                          N => N,
                                          Q => Q,
                                          T => T,
                                          ZpZ       => ZpZ,
                                          Element   => Element,
                                          Zero      => Zero,
                                          One       => One,
                                          Generator => A_Generator,
                                          Polynomial      => Polynomial,
                                          Zero_Polynomial => Zero_Polynomial);

   use BCH_Code_P_N_T;


   type Horla is new Integer range 1..T - 1;

   package GF_P_N_Matrices is new Matrices(Index  => Horla,
                                           Scalar => Element,
                                           Zero   => Zero,
                                           One    => One);

   use GF_P_N_Matrices;


   subtype Positive_Word_Index is Word_Index range 1..Word_Index'Last;

   type Word_Indices is array (Horla) of Positive_Word_Index;

   subtype Horla_Vector is Vector(Horla);


   Lambda               : Element renames BCH_Code_P_N_T.GF_Generator;
   Lambda_K             : array (Horla) of Element;       -- Lambda_K(J) = Lambda ** J

   Minus_Ones           : constant Horla_Vector := (others => -One);

   Deg_G                : constant Degree := Deg(Generator_Polynomial);


   function All_In_ZpZ (Elements : Vector) return Boolean;
      -- True iff In_ZpZ(Elements(I)) for all I

   function To_Word (Indices : Word_Indices;
                     Values  : Horla_Vector) return Word;
      -- Returns the word represented by Indices and Values

   procedure Put (P : in Polynomial);
      -- Writes P in a readable form


   pragma Inline(All_In_ZpZ);

end Check_BCH_Bound_Common;
