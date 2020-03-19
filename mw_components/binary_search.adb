-- GENERIC BINARY SEARCH PACKAGE
   -----------------------------

-- Last Modified By: Mats Weber
-- Last Modified On: Tue Oct 14 16:50:12 1997
-- Update Count    : 3

-- Creation :  8-JUN-1988 by Mats Weber.


package body Binary_Search is
--------------------------

   function Locate (Key    : Key_Type;
                    Within : Item_Array) return Location is
   begin
      if Within'Length = 0 then
         return (Kind => Null_Array);
      else
         declare

            Left   : Index := Within'First;
            Right  : Index := Within'Last;
            Middle : Index;

         begin
            loop
               Middle := Index'Val((Index'Pos(Left) + Index'Pos(Right)) / 2);
               if Key_Of(Within(Middle)) = Key then
                  return (Kind => Found, Position => Middle);
               elsif Key_Of(Within(Middle)) < Key then
                  if Middle = Within'Last then
                     return (Kind => Greater);
                  end if;
                  Left := Index'Succ(Middle);
               else
                  if Middle = Within'First then
                     return (Kind => Smaller);
                  end if;
                  Right := Index'Pred(Middle);
               end if;
               exit when Left > Right;
            end loop;
            return (Kind   => Inbetween,
                    Before => Right,
                    After  => Left);
         end;
      end if;
   end Locate;


   function Locate (Key    : Key_Type;
                    Within : Item_Array) return Index is

      Where : constant Location := Locate(Key, Within);

   begin
      if Where.Kind = Found then
         return Where.Position;
      else
         raise Not_Found;
      end if;
   end Locate;

end Binary_Search;
