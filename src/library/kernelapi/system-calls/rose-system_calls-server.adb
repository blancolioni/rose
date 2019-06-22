with Rose.Words;
with Rose.System_Calls.Client;

package body Rose.System_Calls.Server is

   -------------------------------
   -- Create_Anonymous_Endpoint --
   -------------------------------

   procedure Create_Anonymous_Endpoint
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id)
   is
      Cap : constant Rose.Capabilities.Capability :=
              Create_Endpoint (Create_Cap, Endpoint_Id);
   begin
      pragma Unreferenced (Cap);
   end Create_Anonymous_Endpoint;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   procedure Create_Endpoint
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0;
      Entry_Cap    : out Rose.Capabilities.Capability;
      Endpoint_Cap : out Rose.Capabilities.Capability)
   is
      use Rose.Invocation;
      use type Rose.Objects.Endpoint_Id;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params :=
        Invocation_Record'
          (Control       =>
             Control_Word'
               (Flags          =>
                  (Send       => True,
                   Block      => True,
                   Recv_Caps  => True,
                   Send_Words => True,
                   others     => False),
                Last_Sent_Word => 2,
                Last_Recv_Cap  => 1,
                others         => <>),
           Cap           => Create_Cap,
           Data          =>
             (Rose.Words.Word (Endpoint_Id mod 2 ** 32),
              Rose.Words.Word (Endpoint_Id / 2 ** 32),
              Rose.Words.Word (Identifier),
              others => 0),
           others        => <>);

      Invoke_Capability (Params);

      Entry_Cap := Params.Caps (0);
      Endpoint_Cap := Params.Caps (1);

   end Create_Endpoint;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Endpoint_Id;
   begin
      return Rose.System_Calls.Client.Get_Capability
        (Create_Cap,
         (Rose.Words.Word (Endpoint_Id mod 2 ** 32),
          Rose.Words.Word (Endpoint_Id / 2 ** 32),
          Rose.Words.Word (Identifier)));
   end Create_Endpoint;

   ------------------------
   -- Create_Receive_Cap --
   ------------------------

   function Create_Receive_Cap
     (Create_Cap   : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
   begin
      return Rose.System_Calls.Client.Get_Capability
        (Create_Cap, (1 => 0));
   end Create_Receive_Cap;

   ------------------------
   -- Create_Receive_Cap --
   ------------------------

   function Create_Receive_Cap
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability
   is
      Entry_Cap    : Rose.Capabilities.Capability;
      Endpoint_Cap : Rose.Capabilities.Capability;
   begin
      Create_Endpoint
        (Create_Cap   => Create_Cap,
         Endpoint_Id  => Endpoint_Id,
         Identifier   => Identifier,
         Entry_Cap    => Entry_Cap,
         Endpoint_Cap => Endpoint_Cap);
      return Entry_Cap;
   end Create_Receive_Cap;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap
     (Cap : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Standard_Delete_Cap);
      Send_Cap (Params, Cap);
      Invoke_Capability (Params);
   end Delete_Cap;

   -----------------
   -- Rescind_Cap --
   -----------------

   procedure Rescind_Cap
     (Cap : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Standard_Rescind_Cap);
      Send_Cap (Params, Cap);
      Invoke_Capability (Params);
   end Rescind_Cap;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
      Send_Data : constant Boolean := Data'Length > 0;
   begin
      Params.Control :=
        (Flags => (Reply      => True,
                   Send_Words => Send_Data,
                   others     => False),
                Last_Sent_Word => 0,
         others         => <>);

      Params.Cap := Cap;
      if Send_Data then
         Params.Control.Last_Sent_Word :=
           Parameter_Word_Index (Data'Length - 1);
         for I in Data'Range loop
            Params.Data (Parameter_Word_Index (I - Data'First)) := Data (I);
         end loop;
      end if;

      Invoke_Capability (Params);
   end Send_Reply;

   ---------------------
   -- Send_Reply_Caps --
   ---------------------

   procedure Send_Reply_Caps
     (Cap  : Rose.Capabilities.Capability;
      Caps : Sent_Caps_Array := No_Sent_Caps)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
      Send   : constant Boolean := Caps'Length > 0;
   begin
      Params.Control :=
        (Flags          => (Reply      => True,
                            Send_Caps  => Send,
                            others     => False),
         Last_Sent_Cap  => 0,
         others         => <>);

      Params.Cap := Cap;
      if Send then
         Params.Control.Last_Sent_Cap :=
           Capability_Index (Caps'Length - 1);
         for I in Caps'Range loop
            Params.Caps (Capability_Index (I - Caps'First)) := Caps (I);
         end loop;
      end if;

      Invoke_Capability (Params);
   end Send_Reply_Caps;


end Rose.System_Calls.Server;
