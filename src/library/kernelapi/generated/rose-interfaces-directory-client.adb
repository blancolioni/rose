with Rose.System_Calls;

package body Rose.Interfaces.Directory.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (Item : Directory_Client;
      Name : String)
   return Rose.Interfaces.Directory.Client.Directory_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Create_Directory);
      --  Name in composite;
      Rose.System_Calls.Send_Text (Params, Name);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Directory.Client.Directory_Client;
      begin
         Rose.Interfaces.Directory.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Item : Directory_Client;
      Name : String)
   return Rose.Interfaces.File.Client.File_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Create_File);
      --  Name in composite;
      Rose.System_Calls.Send_Text (Params, Name);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.File.Client.File_Client;
      begin
         Rose.Interfaces.File.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Create_File;

   ---------------------------
   -- Directory_Entry_Count --
   ---------------------------

   function Directory_Entry_Count (Item : Directory_Client) return Natural is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Directory_Entry_Count);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Natural;
      begin
         Result := Natural (Rose.System_Calls.Get_Word_32 (Params, 0));
         return Result;
      end;
   end Directory_Entry_Count;

   --------------------------
   -- Directory_Entry_Kind --
   --------------------------

   function Directory_Entry_Kind
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.Directory.File_Kind
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Directory_Entry_Kind);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Directory.File_Kind;
      begin
         Result := Rose.Interfaces.Directory.File_Kind'Val (Params.Data (0));
         return Result;
      end;
   end Directory_Entry_Kind;

   --------------------------
   -- Directory_Entry_Name --
   --------------------------

   procedure Directory_Entry_Name
     (Item   : in     Directory_Client;
      Index  : in     Positive;
      Result :    out String;
      Last   :    out Natural)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Directory_Entry_Name);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Buffer (Params, Result'Length);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Rose.System_Calls.Copy_Text
        (Params,
         Natural (Params.Data (0)),
         Result,
         Last);
   end Directory_Entry_Name;

   --------------------------
   -- Directory_Entry_Size --
   --------------------------

   function Directory_Entry_Size
     (Item  : Directory_Client;
      Index : Positive)
   return System.Storage_Elements.Storage_Count
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Directory_Entry_Size);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : System.Storage_Elements.Storage_Count;
      begin
         Result := System.Storage_Elements.Storage_Count
            (Rose.System_Calls.Get_Word_32 (Params, 0));
         return Result;
      end;
   end Directory_Entry_Size;

   ----------------
   -- Find_Entry --
   ----------------

   function Find_Entry
     (Item : Directory_Client;
      Name : String)
   return Natural
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Find_Entry);
      --  Name in composite;
      Rose.System_Calls.Send_Text (Params, Name);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Natural;
      begin
         Result := Natural (Rose.System_Calls.Get_Word_32 (Params, 0));
         return Result;
      end;
   end Find_Entry;

   ------------------------------
   -- Get_Create_Directory_Cap --
   ------------------------------

   function Get_Create_Directory_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Create_Directory);

   -------------------------
   -- Get_Create_File_Cap --
   -------------------------

   function Get_Create_File_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Create_File);

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.Directory.Client.Directory_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Directory);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Directory.Client.Directory_Client;
      begin
         Rose.Interfaces.Directory.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Get_Directory;

   -----------------------------------
   -- Get_Directory_Entry_Count_Cap --
   -----------------------------------

   function Get_Directory_Entry_Count_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Directory_Entry_Count);

   ----------------------------------
   -- Get_Directory_Entry_Kind_Cap --
   ----------------------------------

   function Get_Directory_Entry_Kind_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Directory_Entry_Kind);

   ----------------------------------
   -- Get_Directory_Entry_Name_Cap --
   ----------------------------------

   function Get_Directory_Entry_Name_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Directory_Entry_Name);

   ----------------------------------
   -- Get_Directory_Entry_Size_Cap --
   ----------------------------------

   function Get_Directory_Entry_Size_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Directory_Entry_Size);

   ------------------------
   -- Get_Find_Entry_Cap --
   ------------------------

   function Get_Find_Entry_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Find_Entry);

   ---------------------------
   -- Get_Get_Directory_Cap --
   ---------------------------

   function Get_Get_Directory_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Directory);

   -------------------------------
   -- Get_Get_Ordinary_File_Cap --
   -------------------------------

   function Get_Get_Ordinary_File_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Ordinary_File);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -----------------------
   -- Get_Ordinary_File --
   -----------------------

   function Get_Ordinary_File
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.File.Client.File_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Ordinary_File);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.File.Client.File_Client;
      begin
         Rose.Interfaces.File.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Get_Ordinary_File;

   -----------------------
   -- Get_Read_File_Cap --
   -----------------------

   function Get_Read_File_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability
   is (Item.Read_File);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Directory_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 10);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Directory_Entry_Count := Params.Caps (0);
      Client.Directory_Entry_Name := Params.Caps (1);
      Client.Directory_Entry_Kind := Params.Caps (2);
      Client.Directory_Entry_Size := Params.Caps (3);
      Client.Find_Entry := Params.Caps (4);
      Client.Get_Ordinary_File := Params.Caps (5);
      Client.Get_Directory := Params.Caps (6);
      Client.Read_File := Params.Caps (7);
      Client.Create_Directory := Params.Caps (8);
      Client.Create_File := Params.Caps (9);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client                :    out Directory_Client;
      Directory_Entry_Count : in     Rose.Capabilities.Capability;
      Directory_Entry_Name  : in     Rose.Capabilities.Capability;
      Directory_Entry_Kind  : in     Rose.Capabilities.Capability;
      Directory_Entry_Size  : in     Rose.Capabilities.Capability;
      Find_Entry            : in     Rose.Capabilities.Capability;
      Get_Ordinary_File     : in     Rose.Capabilities.Capability;
      Get_Directory         : in     Rose.Capabilities.Capability;
      Read_File             : in     Rose.Capabilities.Capability;
      Create_Directory      : in     Rose.Capabilities.Capability;
      Create_File           : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Directory_Entry_Count := Directory_Entry_Count;
      Client.Directory_Entry_Name := Directory_Entry_Name;
      Client.Directory_Entry_Kind := Directory_Entry_Kind;
      Client.Directory_Entry_Size := Directory_Entry_Size;
      Client.Find_Entry := Find_Entry;
      Client.Get_Ordinary_File := Get_Ordinary_File;
      Client.Get_Directory := Get_Directory;
      Client.Read_File := Read_File;
      Client.Create_Directory := Create_Directory;
      Client.Create_File := Create_File;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Read_File);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      begin
         Rose.Interfaces.Stream_Reader.Client.Open
           (Result,
            Params.Caps (0));
         return Result;
      end;
   end Read_File;

end Rose.Interfaces.Directory.Client;
