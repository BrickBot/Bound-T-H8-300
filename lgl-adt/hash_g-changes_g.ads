generic

package Hash_G.Changes_G is

   procedure  Merge ( Result         : in out Hash_Table_Type;
                      Source         : in     Hash_Table_Type;
                      Prty_At_Result : in     Boolean );
   -- Merges the two tables 'Result' and 'Source', and returns the
   -- resulting table in 'Result'. If 'Result' is not large enough,
   -- then the exception 'Table_Full_Error' is raised, and nothing is
   -- changed. Keys which are contained in both
   -- tables, are treatened as follows: If 'Prty_At_Result' is TRUE,
   -- the data of 'Result' is not changed. If 'Prty_At_Result'  is
   -- FALSE, the data of the 'Source' table is accepted.

   procedure Resize 
         ( Hash_Table   : in out Hash_Table_Type;
           Minimum_Size : in     Positive );
   -- Resizes the hash table to the specified size. If Minimum_Size is not
   -- large enough to store all the elements then the minimum size to
   -- store them is chosen.



end Hash_G.Changes_G;
