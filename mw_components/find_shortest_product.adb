-- GENERIC FUNCTION FOR FINDING THE SHORTEST POSSIBLE PRODUCT SATISFYING A CONDITION
   ---------------------------------------------------------------------------------

-- Creation : 22-APR-1989 by Mats Weber.


function Find_Shortest_Product (Among : Item_Table) return Table_Index_List is
------------------------------
begin
   for Length in 1..List_Index'Last loop
      declare

         Result        : Table_Index_List(1..Length);
         Result_Found  : exception;

         procedure Set_Result (Pos : in List_Index; Product : in Item) is
         begin
            if Pos < Result'Last then
               for I in Among'Range loop
                  Result(Pos) := I;
                  Set_Result(Pos => Pos + 1, Product => Product * Among(I));
               end loop;
            else
               for I in Among'Range loop
                  Result(Pos) := I;
                  if Condition(Product * Among(I)) then
                     raise Result_Found;
                  end if;
               end loop;
            end if;
         end Set_Result;

      begin
         if 1 < Result'Last then
            for I in Among'Range loop
               Result(1) := I;
               Set_Result(Pos => 2, Product => Among(I));
            end loop;
         else
            for I in Among'Range loop
               Result(1) := I;
               if Condition(Among(I)) then
                  raise Result_Found;
               end if;
            end loop;
         end if;
      exception
         when Result_Found =>
            return Result;
      end;
   end loop;
end Find_Shortest_Product;
