with Rose.Server;
with Rose.Interfaces.Directory;
with System.Storage_Elements;
with Rose.Capabilities;

package Rose.Interfaces.Directory.Server is

   function Get_Directory_Entry_Count_Cap
      return Rose.Capabilities.Capability;
   function Get_Directory_Entry_Name_Cap return Rose.Capabilities.Capability;
   function Get_Directory_Entry_Kind_Cap return Rose.Capabilities.Capability;
   function Get_Directory_Entry_Size_Cap return Rose.Capabilities.Capability;
   function Get_Find_Entry_Cap return Rose.Capabilities.Capability;
   function Get_Get_Ordinary_File_Cap return Rose.Capabilities.Capability;
   function Get_Get_Directory_Cap return Rose.Capabilities.Capability;
   function Get_Read_File_Cap return Rose.Capabilities.Capability;
   function Get_Create_Directory_Cap return Rose.Capabilities.Capability;
   function Get_Create_File_Cap return Rose.Capabilities.Capability;

   type Directory_Entry_Count_Handler is access
     function (Id : Rose.Objects.Capability_Identifier) return Natural;

   type Directory_Entry_Name_Handler is access
     procedure
       (Id     : in     Rose.Objects.Capability_Identifier;
        Index  : in     Positive;
        Result :    out String;
        Last   :    out Natural);

   type Directory_Entry_Kind_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Index : Positive)
     return Rose.Interfaces.Directory.File_Kind;

   type Directory_Entry_Size_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Index : Positive)
     return System.Storage_Elements.Storage_Count;

   type Find_Entry_Handler is access
     function
       (Id   : Rose.Objects.Capability_Identifier;
        Name : String)
     return Natural;

   type Get_Ordinary_File_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Index : Positive)
     return Rose.Capabilities.Capability;

   type Get_Directory_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Index : Positive)
     return Rose.Capabilities.Capability;

   type Read_File_Handler is access
     function
       (Id    : Rose.Objects.Capability_Identifier;
        Index : Positive)
     return Rose.Capabilities.Capability;

   type Create_Directory_Handler is access
     function
       (Id   : Rose.Objects.Capability_Identifier;
        Name : String)
     return Rose.Capabilities.Capability;

   type Create_File_Handler is access
     function
       (Id   : Rose.Objects.Capability_Identifier;
        Name : String)
     return Rose.Capabilities.Capability;

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
      Instanced             : in     Boolean := False);

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
      Instanced             : in     Boolean := False);

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
      Create_File           : in     Create_File_Handler);

private

end Rose.Interfaces.Directory.Server;
