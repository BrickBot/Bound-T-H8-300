-- GENERIC PACKAGE FOR GENERATING UNIQUE KEYS (PROTECTED BY A HIDDEN TASK)
   -----------------------------------------------------------------------

-- Revision : 13-Nov-1996 by Mats Weber, added function Image.

-- Creation :  9-Jul-1992 by Mats Weber.


with Keys;

generic
package Protected_Keys is
----------------------

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

   procedure Kill;
      -- Terminates the protecting task.

   Uninitialized_Key,
   Too_Many_Keys       : exception;

private

   package Keys_Instance is new Keys;

   type Key is new Keys_Instance.Key;

   Null_Key : constant Key := Key(Keys_Instance.Null_Key);

   pragma Inline(New_Key, "<", Is_Initialized, Check, Image);

end Protected_Keys;
