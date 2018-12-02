with System.Storage_Elements;

with Rose.Interfaces.File_System.Client;
with Rose.Interfaces.Stream_Reader.Client;

private with Rose.Interfaces.Directory.Client;

package Rose.Directories is

   procedure Open_Root_File_System
     (Client : Rose.Interfaces.File_System.Client.File_System_Client);

   procedure Set_Directory (Directory : String);

   type File_Kind is (Directory, Ordinary_File, Special_File);

   type File_Size is new System.Storage_Elements.Storage_Count;

   function Exists (Name : String) return Boolean;
   function Kind (Name : String) return File_Kind;
   function Size (Name : String) return File_Size;

   procedure Open
     (File : out Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Name : String);

   type Directory_Entry_Type is limited private;

   type Filter_Type is array (File_Kind) of Boolean;

   type Search_Type is limited private;

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True));

   procedure End_Search (Search : in out Search_Type);

   function More_Entries (Search : Search_Type) return Boolean;

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type);

   procedure Search
     (Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True);
      Process   : not null access procedure
        (Directory_Entry : Directory_Entry_Type));

   procedure Simple_Name
     (Directory_Entry : Directory_Entry_Type;
      Name            : out String;
      Last            : out Natural);

   procedure Full_Name
     (Directory_Entry : Directory_Entry_Type;
      Name            : out String;
      Last            : out Natural);

   function Kind
     (Directory_Entry : Directory_Entry_Type)
      return File_Kind;

   function Size
     (Directory_Entry : Directory_Entry_Type)
      return File_Size;

   procedure Open
     (File            : out
        Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Directory_Entry : Directory_Entry_Type);

private

   use Rose.Interfaces.Directory.Client;

   subtype Entry_Name_Type is String (1 .. 400);

   type Directory_Entry_Type is
      record
         Containing_Directory : Directory_Client;
         Directory_Name       : Entry_Name_Type   := (others => ' ');
         Directory_Name_Last  : Natural           := 0;
         Simple_Name          : Entry_Name_Type   := (others => ' ');
         Simple_Name_Last     : Natural           := 0;
         Entry_Index          : Natural           := 0;
         Valid                : Boolean           := False;
         Kind                 : File_Kind         := Special_File;
      end record;

   type Search_Type is limited
      record
         Directory      : Directory_Entry_Type;
         Finished       : Boolean;
         Pattern        : String (1 .. 100);
         Pattern_Length : Natural;
         Filter         : Filter_Type;
      end record;

end Rose.Directories;
