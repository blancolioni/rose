with Rose.Objects;

with Rose.Interfaces.Constructor.Server;
with Rose.Interfaces.Cap_Set.Server;
with Rose.Interfaces.Cap.Server;

with Rose.Server;
with Rose.System_Calls.Server;

package body Caps.Server is

   Max_Cap_Set_Size : constant := 30;

   type Cap_Set_Entry is
      record
         Active : Boolean := False;
         Id     : Rose.Objects.Capability_Identifier;
         Set    : Rose.Capabilities.Capability_Array (1 .. Max_Cap_Set_Size);
         First  : Natural;
         Last   : Natural;
      end record;

   Max_Cap_Set_Count : constant := 100;
   type Cap_Set_Count is range 0 .. Max_Cap_Set_Count;
   subtype Cap_Set_Index is Cap_Set_Count range 1 .. Cap_Set_Count'Last;

   Cap_Sets : array (Cap_Set_Index) of Cap_Set_Entry;

   Next_Cap_Id : Rose.Objects.Capability_Identifier := 1;

   Server      : Rose.Server.Server_Context;

   function Next_Id return Rose.Objects.Capability_Identifier;

   function Create
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Insert
     (Id   : Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array);

   function Take_Next
     (Id   : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Destroy (Id : Rose.Objects.Capability_Identifier);

   function Get_Cap_Set_Object_Id
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id;

   ------------
   -- Create --
   ------------

   function Create
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      for Index in Cap_Sets'Range loop
         if not Cap_Sets (Index).Active then
            declare
               Cap_Set_Id : constant Rose.Objects.Capability_Identifier :=
                              Next_Id;
            begin
               Cap_Sets (Index) :=
                 Cap_Set_Entry'
                   (Active => True,
                    Id     => Cap_Set_Id,
                    Set    => (others => Rose.Capabilities.Null_Capability),
                    First  => 1,
                    Last   => 0);
               return Rose.System_Calls.Server.Create_Endpoint
                 (Create_Cap  => Create_Endpoint_Cap,
                  Endpoint_Id => Rose.Interfaces.Cap_Set.Cap_Set_Interface,
                  Identifier  => Cap_Set_Id);
            end;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Create;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Constructor.Server.Create_Server
        (Server, Create'Access);

      Rose.Interfaces.Cap_Set.Server.Attach_Interface
        (Server_Context => Server,
         Insert         => Insert'Access,
         Take_Next      => Take_Next'Access,
         Instanced      => True);

      Rose.Interfaces.Cap.Server.Attach_Interface
        (Server_Context => Server,
         Destroy        => Destroy'Access,
         Get_Object_Id  => Get_Cap_Set_Object_Id'Access,
         Instanced      => True);

   end Create_Server;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : Rose.Objects.Capability_Identifier) is
   begin
      if Id in 1 .. Max_Cap_Set_Count then
         Cap_Sets (Cap_Set_Index (Id)).Active := False;
      end if;
   end Destroy;

   ---------------------------
   -- Get_Cap_Set_Object_Id --
   ---------------------------

   function Get_Cap_Set_Object_Id
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id
   is
      pragma Unreferenced (Id);
   begin
      return 0;
   end Get_Cap_Set_Object_Id;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Id   : Rose.Objects.Capability_Identifier;
      Caps : Rose.Capabilities.Capability_Array)
   is
   begin
      if Id not in 1 .. Max_Cap_Set_Count then
         return;
      end if;

      declare
         Set : Cap_Set_Entry renames Cap_Sets (Cap_Set_Index (Id));
      begin
         for Cap of Caps loop
            exit when Set.Last = Max_Cap_Set_Size;
            Set.Last := Set.Last + 1;
            Set.Set (Set.Last) := Cap;
         end loop;
      end;
   end Insert;

   -------------
   -- Next_Id --
   -------------

   function Next_Id return Rose.Objects.Capability_Identifier is
      use type Rose.Objects.Capability_Identifier;
   begin
      return Id : constant Rose.Objects.Capability_Identifier :=
        Next_Cap_Id
      do
         Next_Cap_Id := Next_Cap_Id + 1;
      end return;
   end Next_Id;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Server);
   end Start_Server;

   ---------------
   -- Take_Next --
   ---------------

   function Take_Next
     (Id   : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
   begin
      if Id not in 1 .. Max_Cap_Set_Count then
         return Rose.Capabilities.Null_Capability;
      end if;

      declare
         Set : Cap_Set_Entry renames Cap_Sets (Cap_Set_Index (Id));
      begin
         if Set.First > Set.Last then
            return Rose.Capabilities.Null_Capability;
         end if;

         return Cap : constant Rose.Capabilities.Capability :=
           Set.Set (Set.First)
         do
            Set.First := Set.First + 1;
         end return;
      end;
   end Take_Next;

end Caps.Server;
