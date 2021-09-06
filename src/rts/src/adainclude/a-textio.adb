with System.Caps;

with Rose.System_Calls.Client;

package body Ada.Text_IO is

   LF : constant Character := Character'Val (10);

   type File_Control_Block is
      record
         Open         : Boolean := False;
         Current_Line : Count := 0;
         Current_Col  : Count := 0;
         Buffer       : File_Buffer := (others => Character'Val (0));
         Buffer_Last  : Natural := 0;
      end record;

   File_Control : array (Open_File_Index) of File_Control_Block;

   Standard_Input_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => In_File,
                       In_Cap  => System.Caps.Standard_Input,
                       Out_Cap => Rose.Capabilities.Null_Capability,
                       Control => 1);

   Standard_Output_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => Out_File,
                       In_Cap  => Rose.Capabilities.Null_Capability,
                       Out_Cap => System.Caps.Standard_Output,
                       Control => 2);

   Current_Input_File  : File_Type := Standard_Input_File;
   Current_Output_File : File_Type := Standard_Output_File;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
      pragma Unreferenced (File);
   begin
      return True;
   end End_Of_File;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_Input_File);
   end End_Of_File;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      Flush (Current_Output_File);
   end Flush;

   -----------
   -- Flush --
   -----------

   procedure Flush (File : File_Type) is
      FCB : File_Control_Block renames File_Control (File.Control);
   begin
      if FCB.Buffer_Last > 0 then
         Rose.System_Calls.Client.Send_String
           (Cap     => File.Out_Cap,
            Message => FCB.Buffer (1 .. FCB.Buffer_Last));
         FCB.Buffer_Last := 0;
      end if;
   end Flush;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Line : out String;
      Last : out Natural)
   is
   begin
      Get_Line (Current_Input_File, Line, Last);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : File_Type;
      Line : out String;
      Last : out Natural)
   is
      pragma Unreferenced (File, Line);
   begin
      Last := 0;
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return String is
   begin
      return Get_Line (Current_Input_File);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (File : File_Type) return String is
      Buffer : String (1 .. 200);
      Last   : Natural;
   begin
      Get_Line (File, Buffer, Last);
      if Last = Buffer'Last then
         return Buffer & Get_Line (File);
      else
         return Buffer (1 .. Last);
      end if;
   end Get_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      New_Line (Current_Output_File);
   end New_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : File_Type) is
   begin
      Put (File, LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Type; Item : Character) is
      FCB : File_Control_Block renames File_Control (File.Control);
   begin
      FCB.Buffer_Last := FCB.Buffer_Last + 1;
      FCB.Buffer (FCB.Buffer_Last) := Item;
      if Item = LF or else FCB.Buffer_Last = Max_Buffer_Length
      then
         Flush (File);
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Character) is
   begin
      Put (Current_Output_File, Item);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : String) is
   begin
      Put (Current_Output_File, Item);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Type; Item : String) is
   begin
      for Ch of Item loop
         Put (File, Ch);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      Put_Line (Current_Output_File, Item);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Type; Item : String) is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   ---------------
   -- Set_Input --
   ---------------

   procedure Set_Input (File : File_Type) is
   begin
      Current_Input_File := File;
   end Set_Input;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : File_Type) is
   begin
      Current_Output_File := File;
   end Set_Output;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Type is
   begin
      return Standard_Input_File;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Standard_Output_File;
   end Standard_Output;

end Ada.Text_IO;
