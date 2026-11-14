
with Rose.System_Calls.Own_Bank;
with Rose.System_Calls;

with Rose.Core.Directory_Client;
with Rose.Core.File_Client;

with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;

package body Tests.Registry is

   Registry : Rose.Interfaces.Commands.Client.Commands_Client;
   Clock_Interface_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Bank_Interface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Screen_Surface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Cap_Sets_Cap        : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Replacer_Cap        : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;

   ----------
   -- Bank --
   ----------

   function Bank return Rose.Capabilities.Capability is
   begin
      return Bank_Interface_Cap;
   end Bank;

   --------------
   -- Cap_Sets --
   --------------

   function Cap_Sets return Rose.Capabilities.Capability is
   begin
      return Cap_Sets_Cap;
   end Cap_Sets;

   -----------
   -- Clock --
   -----------

   function Clock return Rose.Capabilities.Capability is
   begin
      return Clock_Interface_Cap;
   end Clock;

   --------------
   -- Commands --
   --------------

   function Commands return Rose.Capabilities.Capability is
   begin
      return Registry.Get_Interface_Cap;
   end Commands;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint return Rose.Capabilities.Capability is
   begin
      return Rose.System_Calls.Create_Endpoint_Capability;
   end Create_Endpoint;

   ------------------------
   -- Directory_Endpoint --
   ------------------------

   function Directory_Endpoint
     (Bank : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      No_Caps : Rose.Capabilities.Capability_Array (1 .. 0);
      Program : Rose.Interfaces.Launch.Launch_Object'Class :=
        Launcher ("directory");
   begin
      return Program.Launch_Command
        ((if Bank = Rose.Capabilities.Null_Capability
          then Rose.System_Calls.Own_Bank.Cap
          else Bank),
         No_Caps);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end Directory_Endpoint;

   -------------------
   -- File_Endpoint --
   -------------------

   function File_Endpoint
     (Bank : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      No_Caps : Rose.Capabilities.Capability_Array (1 .. 0);
      Program : Rose.Interfaces.Launch.Launch_Object'Class :=
        Launcher ("file");
   begin
      return Program.Launch_Command
        ((if Bank = Rose.Capabilities.Null_Capability
          then Rose.System_Calls.Own_Bank.Cap
          else Bank),
         No_Caps);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end File_Endpoint;

   ------------
   -- Launch --
   ------------

   function Launch
     (Name : String;
      Caps : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      Program : Rose.Interfaces.Launch.Launch_Object'Class :=
        Launcher (Name);
      Process : Rose.Interfaces.Process.Process_Object'Class :=
        Program.Launch (Rose.System_Calls.Own_Bank.Cap, Caps);
      use type Rose.Capabilities.Capability;
   begin

      --  Elf answers a launch that fails with the null cap, for example
      --  when the kernel's process table is full (#724).

      if Process.Process_Cap = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Process.Published_Interface;
   end Launch;

   ----------------------
   -- Launch_Directory --
   ----------------------

   function Launch_Directory
     (Bank : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Endpoint : constant Rose.Capabilities.Capability :=
        Directory_Endpoint (Bank);
      Served   : Rose.Capabilities.Capability;
      Control  : Rose.Capabilities.Capability;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;
      Start_Directory (Endpoint, Served, Control);
      return Served;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end Launch_Directory;

   -----------------
   -- Launch_File --
   -----------------

   function Launch_File
     (Bank : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      Endpoint : constant Rose.Capabilities.Capability :=
        File_Endpoint (Bank);
      Served   : Rose.Capabilities.Capability;
      Control  : Rose.Capabilities.Capability;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;
      Start_File (Endpoint, Served, Control);
      return Served;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end Launch_File;

   --------------
   -- Launcher --
   --------------

   function Launcher
     (Name : String)
      return Rose.Interfaces.Launch.Launch_Object'Class
   is
   begin
      return Registry.Find (Name);
   end Launcher;

   ------------------
   -- Launcher_Cap --
   ------------------

   function Launcher_Cap
     (Name : String)
      return Rose.Capabilities.Capability
   is
      Program : Rose.Interfaces.Launch.Launch_Object'Class :=
        Launcher (Name);
   begin
      return Program.Launch_Cap;
   end Launcher_Cap;

   --------------
   -- Replacer --
   --------------

   function Replacer return Rose.Capabilities.Capability is
   begin
      return Replacer_Cap;
   end Replacer;

   ------------
   -- Screen --
   ------------

   function Screen return Rose.Capabilities.Capability is
   begin
      return Screen_Surface_Cap;
   end Screen;

   --------------
   -- Set_Bank --
   --------------

   procedure Set_Bank (Bank : Rose.Capabilities.Capability) is
   begin
      Bank_Interface_Cap := Bank;
   end Set_Bank;

   ------------------
   -- Set_Cap_Sets --
   ------------------

   procedure Set_Cap_Sets (Cap_Sets : Rose.Capabilities.Capability) is
   begin
      Cap_Sets_Cap := Cap_Sets;
   end Set_Cap_Sets;

   ---------------
   -- Set_Clock --
   ---------------

   procedure Set_Clock (Clock : Rose.Capabilities.Capability) is
   begin
      Clock_Interface_Cap := Clock;
   end Set_Clock;

   ------------------
   -- Set_Commands --
   ------------------

   procedure Set_Commands
     (Commands : in out Rose.Interfaces.Commands.Commands_Object'Class)
   is
   begin
      Registry.Open (Commands.Commands_Cap);
   end Set_Commands;

   ------------------
   -- Set_Replacer --
   ------------------

   procedure Set_Replacer (Replacer : Rose.Capabilities.Capability) is
   begin
      Replacer_Cap := Replacer;
   end Set_Replacer;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen (Screen : Rose.Capabilities.Capability) is
   begin
      Screen_Surface_Cap := Screen;
   end Set_Screen;

   ---------------------
   -- Start_Directory --
   ---------------------

   procedure Start_Directory
     (Endpoint : Rose.Capabilities.Capability;
      Served   : out Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability;
      Previous : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
   is
      use type Rose.Capabilities.Capability;
      Fresh : constant Boolean :=
        Previous = Rose.Capabilities.Null_Capability;

      function Given
        (Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability
      is (if Fresh then Cap else Rose.Capabilities.Null_Capability);
      --  A new version takes its caps from the old one (#901).

   begin
      Rose.Core.Directory_Client.Call
        (Endpoint         => Endpoint,
         Create_Endpoint  => Given (Create_Endpoint),
         Delete_Cap       => Given (Rose.System_Calls.Delete_Cap_Capability),
         Cap_Set_Service  => Given (Cap_Sets_Cap),
         File_Launch      =>
           (if Fresh then Launcher_Cap ("file")
            else Rose.Capabilities.Null_Capability),
         Directory_Launch =>
           (if Fresh then Launcher_Cap ("directory")
            else Rose.Capabilities.Null_Capability),
         Served           => Served,
         Control          => Control,
         Previous         => Previous);
   end Start_Directory;

   ----------------
   -- Start_File --
   ----------------

   procedure Start_File
     (Endpoint : Rose.Capabilities.Capability;
      Served   : out Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability;
      Previous : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability)
   is
      use type Rose.Capabilities.Capability;
      Fresh : constant Boolean :=
        Previous = Rose.Capabilities.Null_Capability;

      function Given
        (Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability
      is (if Fresh then Cap else Rose.Capabilities.Null_Capability);
      --  A new version takes its caps from the old one (#901), so a
      --  handover passes Previous alone.

   begin
      Rose.Core.File_Client.Call
        (Endpoint        => Endpoint,
         Create_Endpoint => Given (Create_Endpoint),
         Delete_Cap      => Given (Rose.System_Calls.Delete_Cap_Capability),
         Cap_Set_Service => Given (Cap_Sets_Cap),
         Served          => Served,
         Control         => Control,
         Previous        => Previous);
   end Start_File;

end Tests.Registry;
