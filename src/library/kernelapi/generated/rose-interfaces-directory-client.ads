with System.Storage_Elements;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.Stream_Reader.Client;
with Rose.Capabilities;
with Rose.Invocation;
with Rose.Interfaces.Directory;

package Rose.Interfaces.Directory.Client is

   type Directory_Client is private;

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
      Create_File           : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Directory_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Directory_Entry_Count (Item : Directory_Client) return Natural;

   procedure Directory_Entry_Name
     (Item   : in     Directory_Client;
      Index  : in     Positive;
      Result :    out String;
      Last   :    out Natural);

   function Directory_Entry_Kind
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.Directory.File_Kind;

   function Directory_Entry_Size
     (Item  : Directory_Client;
      Index : Positive)
   return System.Storage_Elements.Storage_Count;

   function Find_Entry
     (Item : Directory_Client;
      Name : String)
   return Natural;

   function Get_Ordinary_File
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.File.Client.File_Client;

   function Get_Directory
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.Directory.Client.Directory_Client;

   function Read_File
     (Item  : Directory_Client;
      Index : Positive)
   return Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;

   function Create_Directory
     (Item : Directory_Client;
      Name : String)
   return Rose.Interfaces.Directory.Client.Directory_Client;

   function Create_File
     (Item : Directory_Client;
      Name : String)
   return Rose.Interfaces.File.Client.File_Client;

   function Get_Interface_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Directory_Entry_Count_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Directory_Entry_Name_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Directory_Entry_Kind_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Directory_Entry_Size_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Find_Entry_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Ordinary_File_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Directory_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Read_File_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Create_Directory_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;

   function Get_Create_File_Cap (Item : Directory_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Directory_Client is
      record
         Is_Open               : Boolean := False;
         Interface_Cap         : Rose.Capabilities.Capability := 0;
         Directory_Entry_Count : Rose.Capabilities.Capability := 0;
         Directory_Entry_Name  : Rose.Capabilities.Capability := 0;
         Directory_Entry_Kind  : Rose.Capabilities.Capability := 0;
         Directory_Entry_Size  : Rose.Capabilities.Capability := 0;
         Find_Entry            : Rose.Capabilities.Capability := 0;
         Get_Ordinary_File     : Rose.Capabilities.Capability := 0;
         Get_Directory         : Rose.Capabilities.Capability := 0;
         Read_File             : Rose.Capabilities.Capability := 0;
         Create_Directory      : Rose.Capabilities.Capability := 0;
         Create_File           : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Directory.Client;
