-- GENERIC PACKAGE FOR GENERATING UNIQUE KEYS
   ------------------------------------------

-- Revision : 13-Nov-1996 by Mats Weber, added function Image.
-- Revision :  9-Jul-1992 by Mats Weber, added constant Null_Key.
-- Revision : 27-MAY-1988 by Mats Weber, added procedure CHECK.

-- Creation : 13-APR-1988 by Mats Weber.


with Largest_Numeric_Types;

generic
package Keys is
------------

   type Key is private;
      -- Type of a key. Two keys are equal if and only if they
      -- result from the same call to NEW_KEY.

   Null_Key : constant Key;
      -- Default initial value for the type KEY.

   function New_Key return Key;
      -- Will raise TOO_MANY_KEYS if no more keys can be created.

   function "<" (Left, Right : Key) return Boolean;
      -- Total ordering function.
      -- Null_Key is the smallest of all possible keys.

   function Is_Initialized (The_Key : Key) return Boolean;
      -- Returns TRUE if and only if the key has been initialized
      -- (i.e. is different from NULL_KEY).

   procedure Check (The_Key : in Key);
      -- Checks if THE_KEY is initialized and
      -- raises UNINITIALIZED_KEY if not.

   function Image (Of_Key : Key) return String;
      -- Returns a unique string representation of Of_Key.

   Uninitialized_Key,
   Too_Many_Keys       : exception;

private

   type Key_Value is
      new Largest_Numeric_Types.Large_Integer
             range 0 .. Largest_Numeric_Types.Large_Integer'Last;

   Null_Key_Value : constant Key_Value := Key_Value'First;

   type Key is
      record
         Value : Key_Value := Null_Key_Value;
      end record;

   Null_Key : constant Key := (Value => Null_Key_Value);

   pragma Inline(New_Key, "<", Is_Initialized, Check, Image);

end Keys;
