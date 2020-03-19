-- Filename        : bags-bounded_operations.adb
-- Description     :
-- Author          : Mats Weber
-- Created On      : Wed Jul  1 16:15:38 1998
-- Last Modified By: Mats Weber
-- Last Modified On: Wed Jul  1 16:48:56 1998
-- Update Count    : 2
-- MODIFIED EXPERIMENTALLY BY N. Holsti, 2003-09-02.


package body Bags.Bounded_Operations is
------------------------------------

   use Implementation;


   generic
      with procedure Link_Action (L : in Link);
   procedure Bounded_Link_Traversal (On_Bag      : in Bag;
                                     First, Last : in Key_Type;
                                     Order       : in Traversal_Order);

   generic
      with procedure Link_Action (L : in Link);
   procedure Lower_Bounded_Link_Traversal (On_Bag : in Bag;
                                           First  : in Key_Type;
                                           Order  : in Traversal_Order);

   generic
      with procedure Link_Action (L : in Link);
   procedure Upper_Bounded_Link_Traversal (On_Bag : in Bag;
                                           Last   : in Key_Type;
                                           Order  : in Traversal_Order);


   procedure Bounded_Link_Traversal (On_Bag      : in Bag;
                                     First, Last : in Key_Type;
                                     Order       : in Traversal_Order) is
   begin
      case Order is
         when Ascending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     declare

                        Le_Last : constant Boolean := not (Last < Key_Of(Z.Val));

                     begin
                        if not (Key_Of(Z.Val) < First) then
                           Traverse_Subtree(Z.Left);
                           if Le_Last then
                              Link_Action(Z);
                           end if;
                        end if;
                        exit when not Le_Last;
                        Z := Z.Right;
                     end;
                  end loop;
               end Traverse_Subtree;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
         when Descending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     declare

                        Ge_First : constant Boolean := not (Key_Of(Z.Val) < First);

                     begin
                        if not (Last < Key_Of(Z.Val)) then
                           Traverse_Subtree(Z.Right);
                           if Ge_First then
                              Link_Action(Z);
                           end if;
                        end if;
                        exit when not Ge_First;
                        Z := Z.Left;
                     end;
                  end loop;
               end Traverse_Subtree;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
      end case;
   end Bounded_Link_Traversal;


   procedure Lower_Bounded_Link_Traversal (On_Bag : in Bag;
                                           First  : in Key_Type;
                                           Order  : in Traversal_Order) is
   begin
      case Order is
         when Ascending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     if not (Key_Of(Z.Val) < First) then
                        Traverse_Subtree(Z.Left);
                        Link_Action(Z);
                     end if;
                     Z := Z.Right;
                  end loop;
               end;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
         when Descending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     Traverse_Subtree(Z.Right);
                     exit when Key_Of(Z.Val) < First;
                     Link_Action(Z);
                     Z := Z.Left;
                  end loop;
               end;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
      end case;
   end Lower_Bounded_Link_Traversal;


   procedure Upper_Bounded_Link_Traversal (On_Bag : in Bag;
                                           Last   : in Key_Type;
                                           Order  : in Traversal_Order) is
   begin
      case Order is
         when Ascending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     Traverse_Subtree(Z.Left);
                     exit when Last < Key_Of(Z.Val);
                     Link_Action(Z);
                     Z := Z.Right;
                  end loop;
               end;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
         when Descending =>
            declare

               procedure Traverse_Subtree (L : in Link) is

                  Z : Link := L;

               begin
                  while Z /= null loop
                     if not (Last < Key_Of(Z.Val)) then
                        Traverse_Subtree(Z.Right);
                        Link_Action(Z);
                     end if;
                     Z := Z.Left;
                  end loop;
               end;

            begin
               Traverse_Subtree(On_Bag.Root);
            end;
      end case;
   end Upper_Bounded_Link_Traversal;


   procedure Bounded_Traversal (On_Bag      : in Bag;
                                First, Last : in Key_Type;
                                Order       : in Traversal_Order := Ascending) is

      procedure Link_Action is new Action_On_Link(Action);

      procedure Traverse is new Bounded_Link_Traversal(Link_Action);

   begin
      Traverse(On_Bag, First, Last, Order);
   end Bounded_Traversal;


   procedure Lower_Bounded_Traversal (On_Bag : in Bag;
                                      First  : in Key_Type;
                                      Order  : in Traversal_Order := Ascending) is

      procedure Link_Action is new Action_On_Link(Action);

      procedure Traverse is new Lower_Bounded_Link_Traversal(Link_Action);

   begin
      Traverse(On_Bag, First, Order);
   end Lower_Bounded_Traversal;


   procedure Upper_Bounded_Traversal (On_Bag : in Bag;
                                      Last   : in Key_Type;
                                      Order  : in Traversal_Order := Ascending) is

      procedure Link_Action is new Action_On_Link(Action);

      procedure Traverse is new Upper_Bounded_Link_Traversal(Link_Action);

   begin
      Traverse(On_Bag, Last, Order);
   end Upper_Bounded_Traversal;


   procedure Bounded_Update_All (Within      : in out Bag;
                                 First, Last : in Key_Type;
                                 Order       : in Traversal_Order := Ascending) is

      procedure Link_Modify is new Modify_On_Link(Modify);

      procedure Update_All is new Bounded_Link_Traversal(Link_Modify);

   begin
      Update_All(Within, First, Last, Order);
   end Bounded_Update_All;


   procedure Lower_Bounded_Update_All (Within : in out Bag;
                                       First  : in Key_Type;
                                       Order  : in Traversal_Order := Ascending) is

      procedure Link_Modify is new Modify_On_Link(Modify);

      procedure Update_All is new Lower_Bounded_Link_Traversal(Link_Modify);

   begin
      Update_All(Within, First, Order);
   end Lower_Bounded_Update_All;


   procedure Upper_Bounded_Update_All (Within : in out Bag;
                                       Last   : in Key_Type;
                                       Order  : in Traversal_Order := Ascending) is

      procedure Link_Modify is new Modify_On_Link(Modify);

      procedure Update_All is new Upper_Bounded_Link_Traversal(Link_Modify);

   begin
      Update_All(Within, Last, Order);
   end Upper_Bounded_Update_All;


end Bags.Bounded_Operations;
