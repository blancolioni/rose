with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;

package body Rose.Interfaces.Directory.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Directory_Entry_Count : Directory_Entry_Count_Handler;
   Directory_Entry_Count_Cap : Rose.Capabilities.Capability := 0;
   Local_Directory_Entry_Name : Directory_Entry_Name_Handler;
   Directory_Entry_Name_Cap : Rose.Capabilities.Capability := 0;
   Local_Directory_Entry_Kind : Directory_Entry_Kind_Handler;
   Directory_Entry_Kind_Cap : Rose.Capabilities.Capability := 0;
   Local_Directory_Entry_Size : Directory_Entry_Size_Handler;
   Directory_Entry_Size_Cap : Rose.Capabilities.Capability := 0;
   Local_Find_Entry : Find_Entry_Handler;
   Find_Entry_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Ordinary_File : Get_Ordinary_File_Handler;
   Get_Ordinary_File_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Directory : Get_Directory_Handler;
   Get_Directory_Cap : Rose.Capabilities.Capability := 0;
   Local_Read_File : Read_File_Handler;
   Read_File_Cap : Rose.Capabilities.Capability := 0;
   Local_Create_Directory : Create_Directory_Handler;
   Create_Directory_Cap : Rose.Capabilities.Capability := 0;
   Local_Create_File : Create_File_Handler;
   Create_File_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Directory_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Directory_Entry_Count (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Directory_Entry_Name (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Directory_Entry_Kind (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Directory_Entry_Size (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Find_Entry (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Ordinary_File (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Directory (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Read_File (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Create_Directory (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Create_File (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context        : in out Rose.Server.Server_Context;
      Directory_Entry_Count : in     Directory_Entry_Count_Handler;
      Directory_Entry_Name  : in     Directory_Entry_Name_Handler;
      Directory_Entry_Kind  : in     Directory_Entry_Kind_Handler;
      Directory_Entry_Size  : in     Directory_Entry_Size_Handler;
      Find_Entry            : in     Find_Entry_Handler;
      Get_Ordinary_File     : in     Get_Ordinary_File_Handler;
      Get_Directory         : in     Get_Directory_Handler;
      Read_File             : in     Read_File_Handler;
      Create_Directory      : in     Create_Directory_Handler;
      Create_File           : in     Create_File_Handler;
      Instanced             : in     Boolean := False)
   is
   begin
      Local_Directory_Entry_Count := Directory_Entry_Count;
      Local_Directory_Entry_Name := Directory_Entry_Name;
      Local_Directory_Entry_Kind := Directory_Entry_Kind;
      Local_Directory_Entry_Size := Directory_Entry_Size;
      Local_Find_Entry := Find_Entry;
      Local_Get_Ordinary_File := Get_Ordinary_File;
      Local_Get_Directory := Get_Directory;
      Local_Read_File := Read_File;
      Local_Create_Directory := Create_Directory;
      Local_Create_File := Create_File;
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Interface,
         Handle_Get_Directory_Interface'Access);
      if not Instanced then
         Directory_Entry_Count_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Count_Endpoint);
         Directory_Entry_Name_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Name_Endpoint);
         Directory_Entry_Kind_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Kind_Endpoint);
         Directory_Entry_Size_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Size_Endpoint);
         Find_Entry_Cap := Rose.Server.Create_Endpoint (Find_Entry_Endpoint);
         Get_Ordinary_File_Cap := Rose.Server.Create_Endpoint
            (Get_Ordinary_File_Endpoint);
         Get_Directory_Cap := Rose.Server.Create_Endpoint
            (Get_Directory_Endpoint);
         Read_File_Cap := Rose.Server.Create_Endpoint (Read_File_Endpoint);
         Create_Directory_Cap := Rose.Server.Create_Endpoint
            (Create_Directory_Endpoint);
         Create_File_Cap := Rose.Server.Create_Endpoint
            (Create_File_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Directory_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Count_Endpoint,
         Handle_Directory_Entry_Count'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Name_Endpoint,
         Handle_Directory_Entry_Name'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Kind_Endpoint,
         Handle_Directory_Entry_Kind'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Size_Endpoint,
         Handle_Directory_Entry_Size'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Find_Entry_Endpoint,
         Handle_Find_Entry'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Ordinary_File_Endpoint,
         Handle_Get_Ordinary_File'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Directory_Endpoint,
         Handle_Get_Directory'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_File_Endpoint,
         Handle_Read_File'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Directory_Endpoint,
         Handle_Create_Directory'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_File_Endpoint,
         Handle_Create_File'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context        : in out Rose.Server.Server_Context;
      Directory_Entry_Count : in     Directory_Entry_Count_Handler;
      Directory_Entry_Name  : in     Directory_Entry_Name_Handler;
      Directory_Entry_Kind  : in     Directory_Entry_Kind_Handler;
      Directory_Entry_Size  : in     Directory_Entry_Size_Handler;
      Find_Entry            : in     Find_Entry_Handler;
      Get_Ordinary_File     : in     Get_Ordinary_File_Handler;
      Get_Directory         : in     Get_Directory_Handler;
      Read_File             : in     Read_File_Handler;
      Create_Directory      : in     Create_Directory_Handler;
      Create_File           : in     Create_File_Handler;
      Instanced             : in     Boolean := False)
   is
   begin
      Local_Directory_Entry_Count := Directory_Entry_Count;
      Local_Directory_Entry_Name := Directory_Entry_Name;
      Local_Directory_Entry_Kind := Directory_Entry_Kind;
      Local_Directory_Entry_Size := Directory_Entry_Size;
      Local_Find_Entry := Find_Entry;
      Local_Get_Ordinary_File := Get_Ordinary_File;
      Local_Get_Directory := Get_Directory;
      Local_Read_File := Read_File;
      Local_Create_Directory := Create_Directory;
      Local_Create_File := Create_File;
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Interface,
         Handle_Get_Directory_Interface'Access);
      if not Instanced then
         Directory_Entry_Count_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Count_Endpoint);
         Directory_Entry_Name_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Name_Endpoint);
         Directory_Entry_Kind_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Kind_Endpoint);
         Directory_Entry_Size_Cap := Rose.Server.Create_Endpoint
            (Directory_Entry_Size_Endpoint);
         Find_Entry_Cap := Rose.Server.Create_Endpoint (Find_Entry_Endpoint);
         Get_Ordinary_File_Cap := Rose.Server.Create_Endpoint
            (Get_Ordinary_File_Endpoint);
         Get_Directory_Cap := Rose.Server.Create_Endpoint
            (Get_Directory_Endpoint);
         Read_File_Cap := Rose.Server.Create_Endpoint (Read_File_Endpoint);
         Create_Directory_Cap := Rose.Server.Create_Endpoint
            (Create_Directory_Endpoint);
         Create_File_Cap := Rose.Server.Create_Endpoint
            (Create_File_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Directory_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Count_Endpoint,
         Handle_Directory_Entry_Count'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Name_Endpoint,
         Handle_Directory_Entry_Name'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Kind_Endpoint,
         Handle_Directory_Entry_Kind'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Size_Endpoint,
         Handle_Directory_Entry_Size'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Find_Entry_Endpoint,
         Handle_Find_Entry'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Ordinary_File_Endpoint,
         Handle_Get_Ordinary_File'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Directory_Endpoint,
         Handle_Get_Directory'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_File_Endpoint,
         Handle_Read_File'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Directory_Endpoint,
         Handle_Create_Directory'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_File_Endpoint,
         Handle_Create_File'Access);
   end Create_Server;

   ------------------------------
   -- Get_Create_Directory_Cap --
   ------------------------------

   function Get_Create_Directory_Cap return Rose.Capabilities.Capability
   is (Create_Directory_Cap);

   -------------------------
   -- Get_Create_File_Cap --
   -------------------------

   function Get_Create_File_Cap return Rose.Capabilities.Capability
   is (Create_File_Cap);

   -----------------------------------
   -- Get_Directory_Entry_Count_Cap --
   -----------------------------------

   function Get_Directory_Entry_Count_Cap return Rose.Capabilities.Capability
   is (Directory_Entry_Count_Cap);

   ----------------------------------
   -- Get_Directory_Entry_Kind_Cap --
   ----------------------------------

   function Get_Directory_Entry_Kind_Cap return Rose.Capabilities.Capability
   is (Directory_Entry_Kind_Cap);

   ----------------------------------
   -- Get_Directory_Entry_Name_Cap --
   ----------------------------------

   function Get_Directory_Entry_Name_Cap return Rose.Capabilities.Capability
   is (Directory_Entry_Name_Cap);

   ----------------------------------
   -- Get_Directory_Entry_Size_Cap --
   ----------------------------------

   function Get_Directory_Entry_Size_Cap return Rose.Capabilities.Capability
   is (Directory_Entry_Size_Cap);

   ------------------------
   -- Get_Find_Entry_Cap --
   ------------------------

   function Get_Find_Entry_Cap return Rose.Capabilities.Capability
   is (Find_Entry_Cap);

   ---------------------------
   -- Get_Get_Directory_Cap --
   ---------------------------

   function Get_Get_Directory_Cap return Rose.Capabilities.Capability
   is (Get_Directory_Cap);

   -------------------------------
   -- Get_Get_Ordinary_File_Cap --
   -------------------------------

   function Get_Get_Ordinary_File_Cap return Rose.Capabilities.Capability
   is (Get_Ordinary_File_Cap);

   -----------------------
   -- Get_Read_File_Cap --
   -----------------------

   function Get_Read_File_Cap return Rose.Capabilities.Capability
   is (Read_File_Cap);

   -----------------------------
   -- Handle_Create_Directory --
   -----------------------------

   procedure Handle_Create_Directory (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Name : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Name);
      for Name'Address use Parameters.Buffer_Address;
      Result : constant Rose.Capabilities.Capability :=
         Local_Create_Directory (Parameters.Identifier, Name);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Create_Directory;

   ------------------------
   -- Handle_Create_File --
   ------------------------

   procedure Handle_Create_File (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Name : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Name);
      for Name'Address use Parameters.Buffer_Address;
      Result : constant Rose.Capabilities.Capability := Local_Create_File
         (Parameters.Identifier, Name);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Create_File;

   ----------------------------------
   -- Handle_Directory_Entry_Count --
   ----------------------------------

   procedure Handle_Directory_Entry_Count (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Natural := Local_Directory_Entry_Count
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Result);
   end Handle_Directory_Entry_Count;

   ---------------------------------
   -- Handle_Directory_Entry_Kind --
   ---------------------------------

   procedure Handle_Directory_Entry_Kind (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Result : constant Rose.Interfaces.Directory.File_Kind :=
         Local_Directory_Entry_Kind (Parameters.Identifier, Index);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters,
         Natural'(Rose.Interfaces.Directory.File_Kind'Pos (Result)));
   end Handle_Directory_Entry_Kind;

   ---------------------------------
   -- Handle_Directory_Entry_Name --
   ---------------------------------

   procedure Handle_Directory_Entry_Name (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Last : Natural := 0;
      Result : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Result);
      for Result'Address use Parameters.Buffer_Address;
   begin
      Local_Directory_Entry_Name
        (Parameters.Identifier,
         Index,
         Result,
         Last);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Last);
   end Handle_Directory_Entry_Name;

   ---------------------------------
   -- Handle_Directory_Entry_Size --
   ---------------------------------

   procedure Handle_Directory_Entry_Size (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Result : constant System.Storage_Elements.Storage_Count :=
         Local_Directory_Entry_Size (Parameters.Identifier, Index);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Storage_Offset (Parameters, Result);
   end Handle_Directory_Entry_Size;

   -----------------------
   -- Handle_Find_Entry --
   -----------------------

   procedure Handle_Find_Entry (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Name : String (1 .. Natural (Parameters.Buffer_Length));
      pragma Import (Ada, Name);
      for Name'Address use Parameters.Buffer_Address;
      Result : constant Natural := Local_Find_Entry (Parameters.Identifier,
         Name);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Result);
   end Handle_Find_Entry;

   --------------------------
   -- Handle_Get_Directory --
   --------------------------

   procedure Handle_Get_Directory (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Result : constant Rose.Capabilities.Capability := Local_Get_Directory
         (Parameters.Identifier, Index);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Get_Directory;

   ------------------------------------
   -- Handle_Get_Directory_Interface --
   ------------------------------------

   procedure Handle_Get_Directory_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Directory_Entry_Count_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Directory_Entry_Count_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Directory_Entry_Count_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Directory_Entry_Count_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Directory_Entry_Count_Endpoint, Identifier);
      else
         Cap := Directory_Entry_Count_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Directory_Entry_Name_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Directory_Entry_Name_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Directory_Entry_Name_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Directory_Entry_Name_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Directory_Entry_Name_Endpoint, Identifier);
      else
         Cap := Directory_Entry_Name_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Directory_Entry_Kind_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Directory_Entry_Kind_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Directory_Entry_Kind_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Directory_Entry_Kind_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Directory_Entry_Kind_Endpoint, Identifier);
      else
         Cap := Directory_Entry_Kind_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Directory_Entry_Size_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Directory_Entry_Size_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Directory_Entry_Size_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Directory_Entry_Size_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Directory_Entry_Size_Endpoint, Identifier);
      else
         Cap := Directory_Entry_Size_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Find_Entry_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Find_Entry_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Find_Entry_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Find_Entry_Endpoint,
               Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Find_Entry_Endpoint, Identifier);
      else
         Cap := Find_Entry_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Get_Ordinary_File_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Get_Ordinary_File_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Ordinary_File_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Ordinary_File_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Ordinary_File_Endpoint, Identifier);
      else
         Cap := Get_Ordinary_File_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Get_Directory_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Get_Directory_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Directory_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Directory_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Directory_Endpoint, Identifier);
      else
         Cap := Get_Directory_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Read_File_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Read_File_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Read_File_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Read_File_Endpoint, Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Read_File_Endpoint, Identifier);
      else
         Cap := Read_File_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Create_Directory_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Create_Directory_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Create_Directory_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Create_Directory_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Create_Directory_Endpoint, Identifier);
      else
         Cap := Create_Directory_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Create_File_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Create_File_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Create_File_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Create_File_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Create_File_Endpoint, Identifier);
      else
         Cap := Create_File_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Directory_Interface;

   ------------------------------
   -- Handle_Get_Ordinary_File --
   ------------------------------

   procedure Handle_Get_Ordinary_File (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Result : constant Rose.Capabilities.Capability :=
         Local_Get_Ordinary_File (Parameters.Identifier, Index);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Get_Ordinary_File;

   ----------------------
   -- Handle_Read_File --
   ----------------------

   procedure Handle_Read_File (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Result : constant Rose.Capabilities.Capability := Local_Read_File
         (Parameters.Identifier, Index);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Cap (Parameters, Result);
   end Handle_Read_File;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context        : in out Rose.Server.Server_Context;
      Directory_Entry_Count : in     Directory_Entry_Count_Handler;
      Directory_Entry_Name  : in     Directory_Entry_Name_Handler;
      Directory_Entry_Kind  : in     Directory_Entry_Kind_Handler;
      Directory_Entry_Size  : in     Directory_Entry_Size_Handler;
      Find_Entry            : in     Find_Entry_Handler;
      Get_Ordinary_File     : in     Get_Ordinary_File_Handler;
      Get_Directory         : in     Get_Directory_Handler;
      Read_File             : in     Read_File_Handler;
      Create_Directory      : in     Create_Directory_Handler;
      Create_File           : in     Create_File_Handler)
   is
   begin
      Local_Directory_Entry_Count := Directory_Entry_Count;
      Local_Directory_Entry_Name := Directory_Entry_Name;
      Local_Directory_Entry_Kind := Directory_Entry_Kind;
      Local_Directory_Entry_Size := Directory_Entry_Size;
      Local_Find_Entry := Find_Entry;
      Local_Get_Ordinary_File := Get_Ordinary_File;
      Local_Get_Directory := Get_Directory;
      Local_Read_File := Read_File;
      Local_Create_Directory := Create_Directory;
      Local_Create_File := Create_File;
      Directory_Entry_Count_Cap := Rose.Server.Create_Endpoint
         (Directory_Entry_Count_Endpoint);
      Directory_Entry_Name_Cap := Rose.Server.Create_Endpoint
         (Directory_Entry_Name_Endpoint);
      Directory_Entry_Kind_Cap := Rose.Server.Create_Endpoint
         (Directory_Entry_Kind_Endpoint);
      Directory_Entry_Size_Cap := Rose.Server.Create_Endpoint
         (Directory_Entry_Size_Endpoint);
      Find_Entry_Cap := Rose.Server.Create_Endpoint (Find_Entry_Endpoint);
      Get_Ordinary_File_Cap := Rose.Server.Create_Endpoint
         (Get_Ordinary_File_Endpoint);
      Get_Directory_Cap := Rose.Server.Create_Endpoint
         (Get_Directory_Endpoint);
      Read_File_Cap := Rose.Server.Create_Endpoint (Read_File_Endpoint);
      Create_Directory_Cap := Rose.Server.Create_Endpoint
         (Create_Directory_Endpoint);
      Create_File_Cap := Rose.Server.Create_Endpoint (Create_File_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Directory_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Interface,
         Handle_Get_Directory_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Count_Endpoint,
         Handle_Directory_Entry_Count'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Name_Endpoint,
         Handle_Directory_Entry_Name'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Kind_Endpoint,
         Handle_Directory_Entry_Kind'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Directory_Entry_Size_Endpoint,
         Handle_Directory_Entry_Size'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Find_Entry_Endpoint,
         Handle_Find_Entry'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Ordinary_File_Endpoint,
         Handle_Get_Ordinary_File'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Directory_Endpoint,
         Handle_Get_Directory'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Read_File_Endpoint,
         Handle_Read_File'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_Directory_Endpoint,
         Handle_Create_Directory'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Create_File_Endpoint,
         Handle_Create_File'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Directory.Server;
