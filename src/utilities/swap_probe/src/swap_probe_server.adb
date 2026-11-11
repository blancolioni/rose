with Rose.Interfaces.Named.Client;
with Rose.Interfaces.Named.Server;
with Rose.Server;

package body Swap_Probe_Server is

   use type Rose.Capabilities.Capability;

   Max_Answer : constant := 16;

   type Probe_Object is
     limited new Rose.Interfaces.Named.Server.Named_Interface
   with null record;

   overriding function Named_Cap
     (Item : in out Probe_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Named.Server.Cap_Of (Item));

   overriding function Name (Item : in out Probe_Object) return String;

   Context    : Rose.Server.Server_Context;
   The_Probe  : aliased Probe_Object;

   Answer      : String (1 .. Max_Answer);
   Answer_Last : Natural := 0;

   Target_Cap : Rose.Capabilities.Capability :=
                  Rose.Capabilities.Null_Capability;

   procedure Set_Answer (Text : String);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Number : Natural;
      Target : Rose.Capabilities.Capability)
   is
      Image : constant String := Natural'Image (Number);
   begin
      Target_Cap := Target;
      Set_Answer (Image (Image'First + 1 .. Image'Last));
      Rose.Interfaces.Named.Server.Attach (Context, The_Probe'Access);
   end Create_Server;

   -------------------
   -- Interface_Cap --
   -------------------

   function Interface_Cap return Rose.Capabilities.Capability is
   begin
      return Rose.Interfaces.Named.Server.Interface_Cap (The_Probe'Access);
   end Interface_Cap;

   ----------
   -- Name --
   ----------

   overriding function Name (Item : in out Probe_Object) return String is
      pragma Unreferenced (Item);
   begin
      return Answer (1 .. Answer_Last);
   end Name;

   ----------------
   -- Set_Answer --
   ----------------

   procedure Set_Answer (Text : String) is
      Last : constant Natural := Natural'Min (Text'Length, Max_Answer);
   begin
      Answer (1 .. Last) := Text (Text'First .. Text'First + Last - 1);
      Answer_Last := Last;
   end Set_Answer;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      if Target_Cap /= Rose.Capabilities.Null_Capability then
         declare
            Target : Rose.Interfaces.Named.Client.Named_Client;
         begin
            Rose.Interfaces.Named.Client.Open (Target, Target_Cap);
            Set_Answer (Rose.Interfaces.Named.Client.Name (Target));
         exception
            when others =>
               Set_Answer ("failed");
         end;
      end if;

      Rose.Server.Start_Server (Context);
   end Start_Server;

end Swap_Probe_Server;
