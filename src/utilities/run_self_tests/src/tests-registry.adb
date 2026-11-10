
with Rose.System_Calls.Own_Bank;
with Rose.System_Calls;

with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Process;

package body Tests.Registry is

   Registry : Rose.Interfaces.Commands.Client.Commands_Client;
   Clock_Interface_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Bank_Interface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Screen_Surface_Cap  : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;

   ----------
   -- Bank --
   ----------

   function Bank return Rose.Capabilities.Capability is
   begin
      return Bank_Interface_Cap;
   end Bank;

   -----------
   -- Clock --
   -----------

   function Clock return Rose.Capabilities.Capability is
   begin
      return Clock_Interface_Cap;
   end Clock;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint return Rose.Capabilities.Capability is
   begin
      return Rose.System_Calls.Create_Endpoint_Capability;
   end Create_Endpoint;

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

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen (Screen : Rose.Capabilities.Capability) is
   begin
      Screen_Surface_Cap := Screen;
   end Set_Screen;

end Tests.Registry;
