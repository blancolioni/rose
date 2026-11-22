
with Rose.System_Calls.Own_Bank;
with Rose.System_Calls;

with Rose.Core.Directory_Client;
with Rose.Core.File_Client;
with Rose.Core.Null_Stream_Client;

with Rose.Interfaces.Commands.Lookup.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Processes.Process_Control;

package body Tests.Registry is

   procedure Start_Null_Stream
     (Served   : out Rose.Capabilities.Capability;
      Null_Dir : out Rose.Capabilities.Capability);
   --  Launch the null stream command, against Run_Self_Tests' own bank,
   --  and return its Stream and its null directory, or null caps if the
   --  launch failed.

   Registry : Rose.Interfaces.Commands.Lookup.Client.Lookup_Client;
   Clock_Interface_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Bank_Interface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Screen_Surface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Cap_Sets_Cap        : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Bank_Cap_Sets_Cap   : Rose.Capabilities.Capability :=
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

   -------------------
   -- Bank_Cap_Sets --
   -------------------

   function Bank_Cap_Sets return Rose.Capabilities.Capability is
   begin
      return Bank_Cap_Sets_Cap;
   end Bank_Cap_Sets;

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
      Program : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
        Launcher ("directory");
   begin
      return Program.Launch_Command
        ((if Bank = Rose.Capabilities.Null_Capability
          then Rose.System_Calls.Own_Bank.Cap
          else Bank));
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
      Program : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
        Launcher ("file");
   begin
      return Program.Launch_Command
        ((if Bank = Rose.Capabilities.Null_Capability
          then Rose.System_Calls.Own_Bank.Cap
          else Bank));
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end File_Endpoint;

   ------------
   -- Launch --
   ------------

   function Launch
     (Name : String)
      return Rose.Capabilities.Capability
   is
      Program : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
        Launcher (Name);
      Process : Rose.Interfaces.Processes.Process_Control
        .Process_Control_Object'Class :=
        Program.Launch (Rose.System_Calls.Own_Bank.Cap);
      use type Rose.Capabilities.Capability;
   begin

      --  Elf answers a launch that fails with the null cap, for example
      --  when the kernel's process table is full (#724).

      if Process.Process_Control_Cap = Rose.Capabilities.Null_Capability then
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

   ---------------------------
   -- Launch_Null_Directory --
   ---------------------------

   function Launch_Null_Directory return Rose.Capabilities.Capability is
      Served   : Rose.Capabilities.Capability;
      Null_Dir : Rose.Capabilities.Capability;
   begin
      Start_Null_Stream (Served, Null_Dir);
      Rose.System_Calls.Delete_Cap (Served);
      return Null_Dir;
   end Launch_Null_Directory;

   ------------------------
   -- Launch_Null_Stream --
   ------------------------

   function Launch_Null_Stream return Rose.Capabilities.Capability is
      Served   : Rose.Capabilities.Capability;
      Null_Dir : Rose.Capabilities.Capability;
   begin
      Start_Null_Stream (Served, Null_Dir);
      Rose.System_Calls.Delete_Cap (Null_Dir);
      return Served;
   end Launch_Null_Stream;

   --------------
   -- Launcher --
   --------------

   function Launcher
     (Name : String)
      return Rose.Interfaces.Processes.Launch.Launch_Object'Class
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
      Program : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
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

   -----------------------
   -- Set_Bank_Cap_Sets --
   -----------------------

   procedure Set_Bank_Cap_Sets (Cap_Sets : Rose.Capabilities.Capability)
   is
   begin
      Bank_Cap_Sets_Cap := Cap_Sets;
   end Set_Bank_Cap_Sets;

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
     (Commands : in out Rose.Interfaces.Commands.Lookup.Lookup_Object'Class)
   is
   begin
      Registry.Open (Commands.Lookup_Cap);
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

      Linking : Rose.Capabilities.Capability;
      --  The directory's Linking_Directory, which no test links into.

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
         Linking          => Linking,
         Previous         => Previous);
      Rose.System_Calls.Delete_Cap (Linking);
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

   -----------------------
   -- Start_Null_Stream --
   -----------------------

   procedure Start_Null_Stream
     (Served   : out Rose.Capabilities.Capability;
      Null_Dir : out Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Capability;
      Program  : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
        Launcher ("null_stream");
      Endpoint : constant Rose.Capabilities.Capability :=
        Program.Launch_Command (Rose.System_Calls.Own_Bank.Cap);
   begin
      Served := Rose.Capabilities.Null_Capability;
      Null_Dir := Rose.Capabilities.Null_Capability;
      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;
      Rose.Core.Null_Stream_Client.Call
        (Endpoint        => Endpoint,
         Create_Endpoint => Create_Endpoint,
         Delete_Cap      => Rose.System_Calls.Delete_Cap_Capability,
         Served          => Served,
         Null_Directory  => Null_Dir);
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Served := Rose.Capabilities.Null_Capability;
         Null_Dir := Rose.Capabilities.Null_Capability;
   end Start_Null_Stream;

end Tests.Registry;
