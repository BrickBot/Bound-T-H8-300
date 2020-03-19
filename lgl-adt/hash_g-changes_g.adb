with Hash_G;
with Ada.Text_IO; use Ada.Text_IO;

package body Hash_G.Changes_G is


   procedure  Merge ( Result         : in out Hash_Table_Type;
                      Source         : in     Hash_Table_Type;
                      Prty_At_Result : in     Boolean ) is

      procedure My_Action ( Key      : in     Key_Type;
                            Data     : in out Data_Type;
                            Continue : in out Boolean ) is
      begin
        if Key_Exists( Result, Key ) then
            if not Prty_At_Result then
               Modify( Result, Key, Data );
            end if;
         else
            Insert( Result, Key, Data );
         end if;
      end My_Action;
	
      procedure My_Visit is new Visit( My_Action );

   begin
      if Size( Result ) < Element_Count( Result ) +
                          Element_Count ( Source ) then
         raise Table_Full_Error;
      end if;
      My_Visit( Source );
   end Merge;



   procedure Resize 
         ( Hash_Table   : in out Hash_Table_Type;
           Minimum_Size : in     Positive ) is
           
      New_Table : Hash_Table_Type;
	
      procedure My_Action ( Key      : in     Key_Type;
                            Data     : in out Data_Type;
                            Continue : in out Boolean ) is
      begin
	 Insert ( New_Table, Key, Data );
      end My_Action;
	
      procedure My_Visit is new Visit( My_Action );

   begin
      if ( Minimum_Size < Hash_Table.Counter ) then
	 New_Table := Create( Hash_Table.Counter );
      else
         New_Table := Create ( Minimum_Size );
      end if;
      My_Visit( Hash_Table );
      Hash_Table := New_Table;
   end Resize;


end Hash_G.Changes_G;









