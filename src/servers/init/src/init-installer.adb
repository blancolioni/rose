with System.Storage_Elements;

with Rose.Limits;

with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Stream_Reader.Client;

with Rose.Invocation;
with Rose.System_Calls;

with Init.Calls;

package body Init.Installer is

   Exec_Region_Length : constant := 2 ** 20;

   Buffer : System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size);

   function Reserve_Storage
     (Storage_Cap : Rose.Capabilities.Capability;
      Size        : Rose.Words.Word_64)
      return Rose.Interfaces.Region.Client.Region_Client;

   procedure Copy_Stream
     (From : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      To   : Rose.Interfaces.Region.Client.Region_Client);

   procedure Copy_Caps
     (From       : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      To         : in out Rose.Invocation.Invocation_Record;
      Create_Cap : Rose.Capabilities.Capability);

   ---------------
   -- Copy_Caps --
   ---------------

   procedure Copy_Caps
     (From       : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      To         : in out Rose.Invocation.Invocation_Record;
      Create_Cap : Rose.Capabilities.Capability)
   is
      use Rose.Interfaces.Stream_Reader.Client;
      use System.Storage_Elements;
      Last        : Storage_Count;
      Storage     : Storage_Array (1 .. 16);
      Layout      : Init.Calls.Array_Of_Words (1 .. 4);
      pragma Import (Ada, Layout);
      for Layout'Address use Storage'Address;
   begin
      loop
         Read (From, Storage, Last);
         exit when Has_Error or else Last < 16;
         Rose.System_Calls.Send_Cap
           (To,
            Init.Calls.Call (Create_Cap, Layout));
      end loop;
   end Copy_Caps;

   -----------------
   -- Copy_Stream --
   -----------------

   procedure Copy_Stream
     (From : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      To   : Rose.Interfaces.Region.Client.Region_Client)
   is
      use System.Storage_Elements;
      use Rose.Interfaces.Stream_Reader.Client;
      use Rose.Interfaces.Region.Client;
      use type Rose.Objects.Object_Id;
      Last   : Storage_Count;
      Page_Base : Rose.Objects.Object_Id;
      Page_Bound : Rose.Objects.Object_Id;
      Next_Page : Rose.Objects.Object_Id;
   begin
      Get_Range
        (To, Page_Base, Page_Bound);
      Next_Page := Page_Base;

      loop
         Read (From, Buffer, Last);
         exit when Last = 0;
         Buffer (Last + 1 .. Buffer'Last) := (others => 0);
         Put (To, Next_Page, Buffer);
         Next_Page := Next_Page + 1;
      end loop;
   end Copy_Stream;

   --------------------------
   -- Install_Exec_Library --
   --------------------------

   function Install_Exec_Library
     (Create_Cap      : Rose.Capabilities.Capability;
      Storage_Cap     : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Objects.Object_Id
   is
      use Rose.Interfaces.Region.Client;
      use Rose.Interfaces.Stream_Reader.Client;
      Region : constant Region_Client :=
                     Reserve_Storage
                       (Reserve_Cap,
                        Rose.Words.Word_64 (Binary_Length));
      Exec_Reader : Stream_Reader_Client;
      Cap_Reader  : Stream_Reader_Client;

      Params      : aliased Rose.Invocation.Invocation_Record;
      Exec_Region : constant Region_Client :=
                      Reserve_Storage
                        (Reserve_Cap, Rose.Words.Word_64 (Exec_Region_Length));
   begin

      Open (Exec_Reader, Binary_Stream);
      Copy_Stream (Exec_Reader, Region);

      Open (Cap_Reader, Cap_Stream);
      Rose.System_Calls.Initialize_Send (Params, Launch_Cap);
      Rose.System_Calls.Send_Cap
        (Params, Get_Interface_Cap (Region));
      Rose.System_Calls.Send_Cap
        (Params, Storage_Cap);
      Copy_Caps (Cap_Reader, Params, Create_Cap);
      Rose.System_Calls.Send_Cap
        (Params, Standard_Output);
      Rose.System_Calls.Send_Cap
        (Params, Launch_Cap);
      Rose.System_Calls.Send_Cap
        (Params, Get_Interface_Cap (Exec_Region));
      Rose.System_Calls.Send_Cap
        (Params, Storage_Cap);
      Rose.System_Calls.Invoke_Capability (Params);

      return Rose.System_Calls.Get_Object_Id (Params, 0);
   end Install_Exec_Library;

   ------------------------
   -- Install_Executable --
   ------------------------

   function Install_Executable
     (Create_Cap    : Rose.Capabilities.Capability;
      Install_Cap   : Rose.Capabilities.Capability;
      Cap_Stream    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Binary_Length : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Binary_Length);
      use Rose.Interfaces.Stream_Reader.Client;
      use System.Storage_Elements;
      Client      : Stream_Reader_Client;
      Storage     : Storage_Array (1 .. 16);
      Layout      : Init.Calls.Array_Of_Words (1 .. 4);
      pragma Import (Ada, Layout);
      for Layout'Address use Storage'Address;
      Params      : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Install_Cap);
      Rose.System_Calls.Send_Cap (Params, Binary_Stream);

      Open (Client, Cap_Stream);
      Copy_Caps (Client, Params, Create_Cap);

      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return 0;
      else
         return Params.Caps (0);
      end if;

   end Install_Executable;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   function Reserve_Storage
     (Storage_Cap : Rose.Capabilities.Capability;
      Size        : Rose.Words.Word_64)
      return Rose.Interfaces.Region.Client.Region_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Storage_Cap);
      Rose.System_Calls.Send_Word (Params, Size);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);


      return Result : Rose.Interfaces.Region.Client.Region_Client do
         if Params.Control.Flags (Rose.Invocation.Send_Caps) then
            Rose.Interfaces.Region.Client.Open
              (Result, Params.Caps (0));
         end if;
      end return;

   end Reserve_Storage;

end Init.Installer;
