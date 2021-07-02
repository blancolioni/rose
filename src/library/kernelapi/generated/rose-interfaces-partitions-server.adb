with Rose.Invocation;                  use Rose.Invocation;
with Rose.System_Calls;                use Rose.System_Calls;
with Rose.Capabilities;

package body Rose.Interfaces.Partitions.Server is

   Server_Instance : Rose.Server.Server_Instance;
   Published_Cap : Rose.Capabilities.Capability := 0;
   Local_Partition_Count : Partition_Count_Handler;
   Partition_Count_Cap : Rose.Capabilities.Capability := 0;
   Local_Get_Partition : Get_Partition_Handler;
   Get_Partition_Cap : Rose.Capabilities.Capability := 0;
   procedure Handle_Get_Partitions_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Partition_Count (Parameters :
      in out Rose.Invocation.Invocation_Record);
   procedure Handle_Get_Partition (Parameters :
      in out Rose.Invocation.Invocation_Record);

   ----------------------
   -- Attach_Interface --
   ----------------------

   procedure Attach_Interface
     (Server_Context  : in out Rose.Server.Server_Context;
      Partition_Count : in     Partition_Count_Handler;
      Get_Partition   : in     Get_Partition_Handler;
      Instanced       : in     Boolean := False)
   is
   begin
      Local_Partition_Count := Partition_Count;
      Local_Get_Partition := Get_Partition;
      Rose.Server.Register_Handler
        (Server_Context,
         Partitions_Interface,
         Handle_Get_Partitions_Interface'Access);
      if not Instanced then
         Partition_Count_Cap := Rose.Server.Create_Endpoint
            (Partition_Count_Endpoint);
         Get_Partition_Cap := Rose.Server.Create_Endpoint
            (Get_Partition_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Partitions_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Partition_Count_Endpoint,
         Handle_Partition_Count'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Partition_Endpoint,
         Handle_Get_Partition'Access);
   end Attach_Interface;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server_Context  : in out Rose.Server.Server_Context;
      Partition_Count : in     Partition_Count_Handler;
      Get_Partition   : in     Get_Partition_Handler;
      Instanced       : in     Boolean := False)
   is
   begin
      Local_Partition_Count := Partition_Count;
      Local_Get_Partition := Get_Partition;
      Rose.Server.Register_Handler
        (Server_Context,
         Partitions_Interface,
         Handle_Get_Partitions_Interface'Access);
      if not Instanced then
         Partition_Count_Cap := Rose.Server.Create_Endpoint
            (Partition_Count_Endpoint);
         Get_Partition_Cap := Rose.Server.Create_Endpoint
            (Get_Partition_Endpoint);
         Rose.Server.Create_Anonymous_Endpoint (Partitions_Interface);
      end if;
      Rose.Server.Register_Handler
        (Server_Context,
         Partition_Count_Endpoint,
         Handle_Partition_Count'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Partition_Endpoint,
         Handle_Get_Partition'Access);
   end Create_Server;

   --------------------------
   -- Handle_Get_Partition --
   --------------------------

   procedure Handle_Get_Partition (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Index : constant Positive := Positive (Rose.System_Calls.Get_Word_32
         (Parameters, 0));
      Partition_Type_Low : Rose.Words.Word_64;
      Partition_Type_High : Rose.Words.Word_64;
      Partition_Flags : Rose.Words.Word_64;
      Start_Address : Rose.Words.Word_64;
      Length : Rose.Words.Word_64;
   begin
      Local_Get_Partition
        (Parameters.Identifier,
         Index,
         Partition_Type_Low,
         Partition_Type_High,
         Partition_Flags,
         Start_Address,
         Length);
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Partition_Type_Low);
      Rose.System_Calls.Send_Word (Parameters, Partition_Type_High);
      Rose.System_Calls.Send_Word (Parameters, Partition_Flags);
      Rose.System_Calls.Send_Word (Parameters, Start_Address);
      Rose.System_Calls.Send_Word (Parameters, Length);
   end Handle_Get_Partition;

   -------------------------------------
   -- Handle_Get_Partitions_Interface --
   -------------------------------------

   procedure Handle_Get_Partitions_Interface (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      use Rose.Capabilities;
      Cap : Capability;
      Identifier : constant Rose.Objects.Capability_Identifier :=
         Parameters.Identifier;
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      if Partition_Count_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Partition_Count_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Partition_Count_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Partition_Count_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Partition_Count_Endpoint, Identifier);
      else
         Cap := Partition_Count_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
      if Get_Partition_Cap = Null_Capability then
         if not Rose.Server.Has_Instance (Server_Instance,
            Get_Partition_Endpoint, Identifier)
         then
            Rose.Server.Set_Instance_Cap
              (Server_Instance,
               Get_Partition_Endpoint,
               Identifier,
               Rose.Server.Create_Endpoint (Get_Partition_Endpoint,
                  Identifier));
         end if;
         Cap := Rose.Server.Get_Instance_Cap (Server_Instance,
            Get_Partition_Endpoint, Identifier);
      else
         Cap := Get_Partition_Cap;
      end if;
      Rose.System_Calls.Send_Cap (Parameters, Cap);
   end Handle_Get_Partitions_Interface;

   ----------------------------
   -- Handle_Partition_Count --
   ----------------------------

   procedure Handle_Partition_Count (Parameters :
      in out Rose.Invocation.Invocation_Record) is
      Result : constant Natural := Local_Partition_Count
         (Parameters.Identifier);
   begin
      Rose.System_Calls.Initialize_Reply (Parameters, Parameters.Reply_Cap);
      Rose.System_Calls.Send_Word (Parameters, Result);
   end Handle_Partition_Count;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Server_Context  : in out Rose.Server.Server_Context;
      Partition_Count : in     Partition_Count_Handler;
      Get_Partition   : in     Get_Partition_Handler)
   is
   begin
      Local_Partition_Count := Partition_Count;
      Local_Get_Partition := Get_Partition;
      Partition_Count_Cap := Rose.Server.Create_Endpoint
         (Partition_Count_Endpoint);
      Get_Partition_Cap := Rose.Server.Create_Endpoint
         (Get_Partition_Endpoint);
      Published_Cap := Rose.Server.Create_Endpoint (Partitions_Interface);
      Rose.Server.Register_Handler
        (Server_Context,
         Partitions_Interface,
         Handle_Get_Partitions_Interface'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Partition_Count_Endpoint,
         Handle_Partition_Count'Access);
      Rose.Server.Register_Handler
        (Server_Context,
         Get_Partition_Endpoint,
         Handle_Get_Partition'Access);
      Rose.Server.Publish_Interface (3, Published_Cap);
   end Publish_Interface;

end Rose.Interfaces.Partitions.Server;
