with Rose.Invocation;
with Rose.Standard_Streams;
with Rose.System_Calls.Client;

package body Ada.Text_IO is

   LF : constant Character := Character'Val (10);

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

   --  The standard files have no caps of their own: Input_Cap and
   --  Output_Cap ask Rose.Standard_Streams for them when they are used,
   --  because a command is only given its streams after elaboration.

   Standard_Input_Control  : constant Open_File_Index := 1;
   Standard_Output_Control : constant Open_File_Index := 2;
   Standard_Error_Control  : constant Open_File_Index := 3;

   Standard_Input_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => In_File,
                       In_Cap  => Rose.Capabilities.Null_Capability,
                       Out_Cap => Rose.Capabilities.Null_Capability,
                       Control => Standard_Input_Control);

   Standard_Output_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => Out_File,
                       In_Cap  => Rose.Capabilities.Null_Capability,
                       Out_Cap => Rose.Capabilities.Null_Capability,
                       Control => Standard_Output_Control);

   Standard_Error_File : constant File_Type :=
                    File_Type'
                      (Is_Open => True,
                       Mode    => Out_File,
                       In_Cap  => Rose.Capabilities.Null_Capability,
                       Out_Cap => Rose.Capabilities.Null_Capability,
                       Control => Standard_Error_Control);

   Current_Input_File  : File_Type := Standard_Input_File;
   Current_Output_File : File_Type := Standard_Output_File;

   function Input_Cap (File : File_Type) return Rose.Capabilities.Capability
   is (if File.Control = Standard_Input_Control
       then Rose.Standard_Streams.Standard_Input
       else File.In_Cap);

   function Output_Cap (File : File_Type) return Rose.Capabilities.Capability
   is (if File.Control = Standard_Output_Control
       then Rose.Standard_Streams.Standard_Output
       elsif File.Control = Standard_Error_Control
       then Rose.Standard_Streams.Standard_Error
       else File.Out_Cap);

   procedure Read_Input_Stream
     (Cap : Rose.Capabilities.Capability;
      FCB : in out File_Control_Block);

   procedure Log (Message : String;
                  X       : Natural)
     with Unreferenced;

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
      Cap : constant Rose.Capabilities.Capability := Output_Cap (File);
   begin
      if FCB.Buffer_Last > 0 then
         --  Text written to a stream the program does not have is lost.
         if Cap /= Rose.Capabilities.Null_Capability then
            Rose.System_Calls.Client.Send_String
              (Cap     => Cap,
               Message => FCB.Buffer (1 .. FCB.Buffer_Last));
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
            Read_Input_Stream (Input_Cap (File), FCB);
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

   ---------
   -- Log --
   ---------

   procedure Log
     (Message : String;
      X       : Natural)
   is
      Buffer : String (1 .. 200);
      Last   : Natural := 0;

      procedure Add (Ch : Character);

      ---------
      -- Add --
      ---------

      procedure Add (Ch : Character) is
      begin
         Last := Last + 1;
         Buffer (Last) := Ch;
      end Add;

   begin
      for Ch of Message loop
         Add (Ch);
      end loop;

      if X = 0 then
         Add ('0');
      else
         declare
            It    : Natural := X;
            Buf   : String (1 .. 20);
            Index : Natural := 0;
         begin
            while It /= 0 loop
               Index := Index + 1;
               Buf (Index) := Character'Val (It mod 10 + 48);
               It := It / 10;
            end loop;
            for I in reverse 1 .. Index loop
               Add (Buf (I));
            end loop;
         end;
      end if;
      Add (Character'Val (10));
      Rose.System_Calls.Client.Send_String
        (Cap     => Output_Cap (Standard_Output_File),
         Message => Buffer (1 .. Last));
   end Log;

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
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Cap = Rose.Capabilities.Null_Capability then
         FCB.Buffer_Last := 0;
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Receive_Buffer (Params, Max_Buffer_Length);
      Rose.System_Calls.Invoke_Capability (Params);
      Rose.System_Calls.Copy_Text
        (Params => Params,
         Count  => Natural (Params.Data (0)),
         To     => FCB.Buffer,
         Last   => FCB.Buffer_Last);
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
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Standard_Error_File;
   end Standard_Error;

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
