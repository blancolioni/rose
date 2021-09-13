with Ada.Text_IO;

package body Ada_Text_IO is

   LF : constant Character := Character'Val (10);

   --  Standard_Files : array (Rose.Capabilities.Capability range 1 .. 2)
   --    of Ada.Text_IO.File_Type;

   type File_Control_Block is
      record
         Open         : Boolean     := False;
         At_End       : Boolean     := False;
         Current_Line : Count       := 0;
         Current_Col  : Count       := 0;
         Buffer       : File_Buffer := (others => Character'Val (0));
         Buffer_First : Positive    := 1;
         Buffer_Last  : Natural     := 0;
      end record;

   File_Control : array (Open_File_Index) of File_Control_Block;

   Standard_Input_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => In_File,
                       In_Cap  => 1,
                       Out_Cap => Rose.Capabilities.Null_Capability,
                       Control => 1);

   Standard_Output_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => Out_File,
                       In_Cap  => Rose.Capabilities.Null_Capability,
                       Out_Cap => 2,
                       Control => 2);

   Current_Input_File  : File_Type := Standard_Input_File;
   Current_Output_File : File_Type := Standard_Output_File;

   procedure Read_Input_Stream
     (Cap : Rose.Capabilities.Capability;
      FCB : in out File_Control_Block);

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return File_Control (File.Control).At_End;
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
      use type Rose.Capabilities.Capability;
      FCB : File_Control_Block renames File_Control (File.Control);
   begin
      if FCB.Buffer_Last > 0 then
         if File.Out_Cap /= 0 then
            Ada.Text_IO.Put (FCB.Buffer (1 .. FCB.Buffer_Last));
         else
            raise Constraint_Error with "out cap = 0";
         end if;
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
      FCB   : File_Control_Block renames File_Control (File.Control);
      Index : Positive renames FCB.Buffer_First;
   begin
      Last := Line'First - 1;
      for Ch of Line loop
         if Index > FCB.Buffer_Last then
            Read_Input_Stream (File.In_Cap, FCB);
            if FCB.Buffer_Last = 0 then
               FCB.At_End := True;
               return;
            end if;
            Index := 1;
         end if;

         if FCB.Buffer (Index) = LF then
            Index := Index + 1;
            exit;
         end if;

         Last := Last + 1;
         Ch := FCB.Buffer (Index);
         Index := Index + 1;

      end loop;

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

   -----------------------
   -- Read_Input_Stream --
   -----------------------

   procedure Read_Input_Stream
     (Cap : Rose.Capabilities.Capability;
      FCB : in out File_Control_Block)
   is
      use type Rose.Capabilities.Capability;
   begin
      if Cap = Rose.Capabilities.Null_Capability then
         FCB.Buffer_Last := 0;
         return;
      end if;

      if Ada.Text_IO.End_Of_File then
         FCB.Buffer_Last := 0;
      end if;

      Ada.Text_IO.Get_Line
        (FCB.Buffer,
         FCB.Buffer_Last);

      if FCB.Buffer_Last < FCB.Buffer'Last then
         FCB.Buffer_Last := FCB.Buffer_Last + 1;
         FCB.Buffer (FCB.Buffer_Last) := Character'Val (10);
      end if;
   end Read_Input_Stream;

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

end Ada_Text_IO;
