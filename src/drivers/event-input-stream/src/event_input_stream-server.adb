with System.Storage_Elements;

with Rose.Objects;
with Rose.Words;

with Rose.Invocation;

with Rose.Interfaces.Event_Listener.Server;
with Rose.Interfaces.Stream_Reader.Server;

with Rose.System_Calls;
with Rose.Server;

--  with Rose.Console_IO;

package body Event_Input_Stream.Server is

   Cooked_Mode  : constant Boolean := True;

   Context         : Rose.Server.Server_Context;

   Input_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   Input_Last   : System.Storage_Elements.Storage_Count := 0;

   procedure On_Event
     (Id   : Rose.Objects.Capability_Identifier;
      Code : Rose.Words.Word);

   procedure On_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

   --------------
   -- On_Event --
   --------------

   procedure On_Event
     (Id   : Rose.Objects.Capability_Identifier;
      Code : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      use type Rose.Words.Word;
      use System.Storage_Elements;
   begin
      if Input_Last < Input_Buffer'Last then
         Input_Last := Input_Last + 1;
         Input_Buffer (Input_Last) :=
           Storage_Element (Code mod 2 ** System.Storage_Unit);
      end if;

      if not Cooked_Mode
        or else Code = 10
        or else Input_Last = Input_Buffer'Last
      then
         --  Rose.Console_IO.Put_Line ("stream: unblocking read");
         Rose.Server.Unblock_Endpoint
           (Context, Rose.Interfaces.Stream_Reader.Read_Endpoint);
         --  Rose.Console_IO.Put_Line ("stream: done");
      end if;
   end On_Event;

   -------------
   -- On_Read --
   -------------

   procedure On_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Id);
      use System.Storage_Elements;
   begin

      --  Rose.Console_IO.Put ("stream: on-read: last=");
      --  Rose.Console_IO.Put (Natural (Input_Last));
      --  Rose.Console_IO.New_Line;

      if Input_Last > 0 then
         Last :=
           Storage_Count'Min (Buffer'Last, Input_Last + Buffer'First - 1);
         --  Rose.Console_IO.Put ("stream: on-read: sending last=");
         --  Rose.Console_IO.Put (Natural (Last));
         --  Rose.Console_IO.New_Line;
         Buffer (Buffer'First .. Last) := Input_Buffer (1 .. Input_Last);
         --  Rose.Console_IO.Put_Line ("stream: on-read: resetting input");
         Input_Last := 0;
      else
         --  Rose.Console_IO.Put_Line ("stream: on-read: blocking");
         Rose.Server.Block_Current_Request (Context);
      end if;
   end On_Read;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin

      Rose.Interfaces.Stream_Reader.Server.Publish_Interface
        (Server_Context => Context,
         Read           => On_Read'Access);

      Rose.Interfaces.Event_Listener.Server.Attach_Interface
        (Server_Context => Context,
         On_Event       => On_Event'Access);

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Event_Source_Cap);
         Rose.System_Calls.Invoke_Capability (Params);

         Rose.System_Calls.Initialize_Send (Params, Params.Caps (0));
         Rose.System_Calls.Send_Cap
           (Params => Params,
            Cap    =>
              Rose.Interfaces.Event_Listener.Server.Get_On_Event_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      Rose.Server.Start_Server (Context);

   end Start_Server;

end Event_Input_Stream.Server;
