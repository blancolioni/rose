private with Rose.Capabilities;

package Ada.Text_IO is

   type Count is range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0;

   type File_Type is limited private;

   type File_Mode is (In_File, Out_File, Append_File);

   procedure Put (File : File_Type; Item : Character);
   procedure Put (Item : Character);

   procedure Put (File : File_Type; Item : String);
   procedure Put (Item : String);

   procedure Put_Line (File : File_Type; Item : String);
   procedure Put_Line (Item : String);

   procedure New_Line (File : File_Type);
   procedure New_Line;

   procedure Flush (File : File_Type);
   procedure Flush;

   function Standard_Output return File_Type;

   procedure Set_Output (File : File_Type);

private

   Max_Open_Files    : constant := 20;
   Max_Buffer_Length : constant := 1024;

   subtype File_Buffer is String (1 .. Max_Buffer_Length);

   type Open_File_Index is range 1 .. Max_Open_Files;

   type File_Type is
      record
         Is_Open       : Boolean := False;
         Mode          : File_Mode;
         In_Cap        : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
         Out_Cap       : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
         Control       : Open_File_Index;
      end record;

end Ada.Text_IO;
