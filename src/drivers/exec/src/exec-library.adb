with System.Storage_Elements;

with Rose.Limits;
with Rose.System_Calls.Server;

with Rose.Interfaces.Launch;

package body Exec.Library is

   Max_Installed_Programs : constant := 100;

   type Installed_Program_Record is
      record
         Installed : Boolean := False;
         ELF_Base  : Rose.Objects.Page_Object_Id;
         ELF_Bound : Rose.Objects.Page_Object_Id;
      end record;

   subtype Installed_Program_Index is
     Rose.Objects.Capability_Identifier range 1 .. Max_Installed_Programs;

   type Installed_Program_Array is
     array (Installed_Program_Index) of Installed_Program_Record;

   Installed_Programs : Installed_Program_Array;
   Installed_Count    : Rose.Objects.Capability_Identifier := 0;

   Space_Bank         : Rose.Interfaces.Space_Bank.Client.Space_Bank_Client;
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
     (ELF_Image    : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client)
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
         use Rose.Interfaces.Space_Bank.Client;
         use type Rose.Objects.Object_Id;
         Buffer : Storage_Array (1 .. Rose.Limits.Page_Size);
         Last   : Storage_Count;
         Install : Installed_Program_Record renames
                     Installed_Programs (Installed_Count);
      begin
         Install.Installed := True;
         Install.ELF_Base := Next_Page;

         loop
            Read (ELF_Image, Buffer, Last);
            exit when Last = 0;
            Buffer (Last + 1 .. Buffer'Last) := (others => 0);
            Put (Space_Bank, Next_Page, Buffer);
            Next_Page := Next_Page + 1;
         end loop;
         Install.ELF_Bound := Next_Page;
      end;

      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap  => Create_Endpoint_Cap,
         Endpoint_Id => Rose.Interfaces.Launch.Launch_Endpoint,
         Identifier  => Installed_Count);
   end Install;

   --------------------
   -- Set_Space_Bank --
   --------------------

   procedure Set_Space_Bank
     (Client : Rose.Interfaces.Space_Bank.Client.Space_Bank_Client)
   is
   begin
      Space_Bank := Client;
      Rose.Interfaces.Space_Bank.Client.Get_Range
        (Space_Bank, Page_Base, Page_Bound);
      Next_Page := Page_Base;
   end Set_Space_Bank;

end Exec.Library;
