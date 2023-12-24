with System.Storage_Elements;

with Rose.Limits;
with Rose.System_Calls.Server;

with Rose.Interfaces.Launch;

package body Exec.Library is

   Max_Installed_Programs : constant := 100;
   Max_Start_Caps         : constant := 16;
   Max_Name_Length        : constant := 16;

   subtype Installed_Program_Name is String (1 .. Max_Name_Length);

   type Installed_Program_Record is
      record
         Installed : Boolean := False;
         Caps      : Rose.Capabilities.Capability_Array (1 .. Max_Start_Caps);
         Name      : Installed_Program_Name;
         ELF_Base  : Rose.Objects.Page_Object_Id;
         ELF_Bound : Rose.Objects.Page_Object_Id;
      end record;

   subtype Installed_Program_Index is
     Rose.Objects.Capability_Identifier range 1 .. Max_Installed_Programs;

   type Installed_Program_Array is
     array (Installed_Program_Index) of Installed_Program_Record;

   Installed_Programs : Installed_Program_Array;
   Installed_Count    : Rose.Objects.Capability_Identifier := 0;

   Region         : Rose.Interfaces.Region.Client.Region_Client;
   Page_Base,
   Page_Bound         : Rose.Objects.Object_Id := 0;
   Next_Page          : Rose.Objects.Object_Id := 0;

   ---------------------
   -- Get_Image_Pages --
   ---------------------

   procedure Get_Image_Pages
     (Id          : Rose.Objects.Capability_Identifier;
      Base, Bound : out Rose.Objects.Page_Object_Id)
   is
   begin
      if Id in Installed_Program_Index
        and then Installed_Programs (Id).Installed
      then
         Base := Installed_Programs (Id).ELF_Base;
         Bound := Installed_Programs (Id).ELF_Bound;
      end if;
   end Get_Image_Pages;

   -------------
   -- Install --
   -------------

   function Install
     (ELF_Image : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Name      : String;
      Caps      : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      if Installed_Count = Max_Installed_Programs then
         return 0;
      end if;

      Installed_Count := Installed_Count + 1;

      declare
         use System.Storage_Elements;
         use Rose.Interfaces.Stream_Reader.Client;
         use Rose.Interfaces.Region.Client;
         use type Rose.Objects.Object_Id;
         Buffer : Storage_Array (1 .. Rose.Limits.Page_Size);
         Last   : Storage_Count;
         Install : Installed_Program_Record renames
                     Installed_Programs (Installed_Count);
      begin
         Install.Installed := True;
         Install.Caps := (others => 0);
         Install.Caps (1 .. Caps'Length) := Caps;

         Install.Name := (others => ' ');

         if Name'Length <= Max_Name_Length then
            Install.Name (1 .. Name'Length) := Name;
         else
            Install.Name :=
              Name (Name'First .. Name'First + Max_Name_Length - 1);
         end if;

         Install.ELF_Base := Next_Page;

         loop
            Read (ELF_Image, Buffer, Last);
            exit when Last = 0;
            Buffer (Last + 1 .. Buffer'Last) := (others => 0);
            Put (Region, Next_Page, Buffer);
            Next_Page := Next_Page + 1;
         end loop;
         Install.ELF_Bound := Next_Page;
      end;

      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap  => Create_Endpoint_Cap,
         Endpoint_Id => Rose.Interfaces.Launch.Launch_Endpoint,
         Identifier  => Installed_Count);
   end Install;

   -----------------------
   -- Send_Install_Caps --
   -----------------------

   procedure Send_Install_Caps
     (Id     : Rose.Objects.Capability_Identifier;
      Params : in out Rose.Invocation.Invocation_Record)
   is
      use type Rose.Capabilities.Capability;
   begin
      if Id in Installed_Program_Index
        and then Installed_Programs (Id).Installed
      then
         for Cap of Installed_Programs (Id).Caps loop
            exit when Cap = 0;
            Rose.Invocation.Send_Cap (Params, Cap);
         end loop;
      end if;
   end Send_Install_Caps;

   --------------------
   -- Set_Region --
   --------------------

   procedure Set_Region
     (Client : Rose.Interfaces.Region.Client.Region_Client)
   is
   begin
      Region := Client;
      Rose.Interfaces.Region.Client.Get_Range
        (Region, Page_Base, Page_Bound);
      Next_Page := Page_Base;
   end Set_Region;

end Exec.Library;
