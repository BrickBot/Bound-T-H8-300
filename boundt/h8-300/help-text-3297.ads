   procedure List_Options (Device : in Device_T)
   is
      use Ada.Strings;
      use Ada.Text_IO;
      use type H8_300.Memory_Location_T;
   begin

      Put_Line ("Options for -device=" & Name (Device) & ':');
      New_Line;
      Put_Line ("   -internal_ram=enabled");
      Put_Line ("      The internal RAM is enabled (bit SYSCR.RAME = 1).");
      Put_Line ("      Internal RAM is faster than external RAM.");

      if Default_RAME then
      Put_Line ("      This is the default.");
      end if;

      New_Line;
      Put_Line ("   -internal_ram=disabled");
      Put_Line ("      The internal RAM is disabled (bit SYSCR.RAME = 0).");
      Put_Line ("      Addresses in this range map to the external RAM");
      Put_Line ("      and access is at the (slower) external RAM speed.");
      Put_Line ("      Implies -stack=external.");

      if not Default_RAME then
      Put_Line ("      This is the default.");
      end if;

      New_Line;
      Put_Line ("   -mode=1");
      Put_Line ("   -mode=2");
      Put_Line ("   -mode=3");
      Put_Line ("      Defines the processor's operating mode:");
      Put_Line ("         1  Expanded mode without on-chip ROM.");
      Put_Line ("         2  Expanded mode with    on-chip ROM.");
      Put_Line ("         3  Single-chip mode (no external memory).");
      Put_Line ("      Mode 3 implies -stack=internal.");
      Put_Line ("      The default is -mode="
                &         Fixed.Trim (Mode_T'Image (Default_Mode), Left)
                &      '.');
      New_Line;

   end List_Options;
