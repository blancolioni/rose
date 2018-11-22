with Ada.Text_IO;

with WL.String_Maps;

with IDL.Types;

package body IDL.Generate_Init is

   package Cap_Maps is
     new WL.String_Maps (Positive);

   package Type_Maps is
     new WL.String_Maps (IDL.Types.IDL_Type, IDL.Types."=");

   package String_Index_Maps is
     new WL.String_Maps (Natural);

   Arg_Caps  : Cap_Maps.Map;
   Locals    : Type_Maps.Map;
   Registers : Cap_Maps.Map;
   Strs      : String_Index_Maps.Map;

   Next_Command_Index : Natural := 0;

   Free : array (1 .. 255) of Boolean :=
            (1 .. 15 => False,
             others  => True);
   Next_Free : Positive := 16;

   procedure Start_Command;
   procedure Finish_Command;

   function Allocate_Register return Natural;
   procedure Deallocate_Register (Register : Positive)
     with Unreferenced;

   procedure Move_Address
     (Target_R       : Natural;
      Source_Address : String);

   procedure Move_Integer
     (Target_R : Natural;
      Value    : Integer);

   procedure Move_String
     (Target_R : Natural;
      Value    : String)
     with Unreferenced;

   procedure Move_String
     (Target_R : Natural;
      Index    : Natural);

   procedure Copy_To_Buffer
     (Buffer_R : Natural;
      Source_R : Natural;
      Length_R : Natural);

   function Image (X : Integer) return String;

   -----------------------
   -- Allocate_Register --
   -----------------------

   function Allocate_Register return Natural is
   begin
      while not Free (Next_Free) loop
         Next_Free := Next_Free + 1;
      end loop;
      Free (Next_Free) := False;
      return Next_Free;
   end Allocate_Register;

   --------------------
   -- Copy_To_Buffer --
   --------------------

   procedure Copy_To_Buffer
     (Buffer_R : Natural;
      Source_R : Natural;
      Length_R : Natural)
   is
   begin
      Ada.Text_IO.Put_Line
        ("           (Command     => Copy,");
      Ada.Text_IO.Put_Line
        ("            To_Buffer   => " & Image (Buffer_R) & ",");
      Ada.Text_IO.Put_Line
        ("            From_Buffer => " & Image (Source_R) & ",");
      Ada.Text_IO.Put
        ("            Length      => " & Image (Length_R) & ")");
   end Copy_To_Buffer;

   -------------------------
   -- Deallocate_Register --
   -------------------------

   procedure Deallocate_Register (Register : Positive) is
   begin
      Free (Register) := False;
   end Deallocate_Register;

   --------------------
   -- Finish_Command --
   --------------------

   procedure Finish_Command is
   begin
      null; --  Ada.Text_IO.Put_Line (",");
   end Finish_Command;

   --------------------------
   -- Generate_Init_Script --
   --------------------------

   procedure Generate_Init_Script
     (Proc : IDL.Procs.IDL_Procedure;
      Path : String)
   is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Set_Output (File);

      Ada.Text_IO.Put_Line
        ("with System.Storage_Elements;");
      Ada.Text_IO.Put_Line
        ("with Init.Commands;                        use Init.Commands;");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("package Init.Script is");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("   type Script_Type is");
      Ada.Text_IO.Put_Line
        ("     array (Positive range <>) of Init.Commands.Command_Record;");
      Ada.Text_IO.New_Line;

      declare
         Index : Natural := 0;

         procedure Save_Arg
           (Name : String;
            Typ  : IDL.Types.IDL_Type);

         --------------
         -- Save_Arg --
         --------------

         procedure Save_Arg
           (Name : String;
            Typ  : IDL.Types.IDL_Type)
         is
            pragma Unreferenced (Typ);
         begin
            Index := Index + 1;
            Arg_Caps.Insert (Name, Index);
            Registers.Insert (Name, Index);
         end Save_Arg;

      begin
         IDL.Procs.Scan_Arguments (Proc, Save_Arg'Access);
      end;

      declare
         procedure Save_Local
           (Name : String;
            Typ  : IDL.Types.IDL_Type);

         ----------------
         -- Save_Local --
         ----------------

         procedure Save_Local
           (Name : String;
            Typ  : IDL.Types.IDL_Type)
         is
         begin
            if IDL.Types.Get_Name (Typ) = "page" then
               Ada.Text_IO.Put_Line
                 ("   " & Name
                  & " : System.Storage_Elements.Storage_Array (1 .. 4096)");
               Ada.Text_IO.Put_Line
                 ("     with Alignment => 4096;");
            end if;
            Locals.Insert (Name, Typ);
            Registers.Insert (Name, Allocate_Register);
         end Save_Local;

      begin
         IDL.Procs.Scan_Locals (Proc, Save_Local'Access);
      end;

      declare

         Next_Index : Natural := 0;

         procedure Save_Invoke_Strings
           (Cap_Name : String;
            Endpoint : Positive;
            Aspects  : IDL.Procs.IDL_Aspects);

         -------------------------
         -- Save_Invoke_Strings --
         -------------------------

         procedure Save_Invoke_Strings
           (Cap_Name : String;
            Endpoint : Positive;
            Aspects  : IDL.Procs.IDL_Aspects)
         is
            pragma Unreferenced (Cap_Name, Endpoint);
            List_Index : Natural := 0;

            procedure Save_Sent_String
              (Expr : IDL.Procs.IDL_Expression);

            ----------------------
            -- Save_Sent_String --
            ----------------------

            procedure Save_Sent_String
              (Expr : IDL.Procs.IDL_Expression)
            is
            begin
               List_Index := List_Index + 1;
               if List_Index = 1 or else List_Index = 2 then
                  null;
               elsif List_Index = 3 then
                  declare
                     Text : constant String :=
                              IDL.Procs.Get_String_Value
                                (IDL.Procs.Evaluate_Static_Expression
                                   (Expr));
                  begin
                     if not Strs.Contains (Text) then
                        Strs.Insert (Text, Next_Index);
                        Ada.Text_IO.Put
                          ("   S" & Image (Next_Index)
                           & " : aliased String :=");
                        declare
                           Start : Positive := Text'First;
                        begin
                           for I in Text'Range loop
                              if Character'Pos (Text (I)) not in 32 .. 127 then
                                 Ada.Text_IO.New_Line;
                                 if Start < I then
                                    if Start > Text'First then
                                       Ada.Text_IO.Put ("     & ");
                                    else
                                       Ada.Text_IO.Put ("       ");
                                    end if;
                                    Ada.Text_IO.Put_Line
                                      ("""" & Text (Start .. I - 1) & """");
                                 end if;
                                 if I > 1 then
                                    Ada.Text_IO.Put ("     & ");
                                 else
                                    Ada.Text_IO.Put ("       ");
                                 end if;
                                 Ada.Text_IO.Put ("Character'Val ("
                                                  & Image
                                                    (Character'Pos (Text (I)))
                                                  & ")");
                                 Start := I + 1;
                              end if;
                           end loop;

                           if Start <= Text'Last then
                              Ada.Text_IO.New_Line;
                              if Start > Text'First then
                                 Ada.Text_IO.Put ("     & ");
                              else
                                 Ada.Text_IO.Put ("       ");
                              end if;
                              Ada.Text_IO.Put
                                ("""" & Text (Start .. Text'Last) & """");
                           end if;

                           Ada.Text_IO.Put_Line (";");
                        end;

                        Next_Index := Next_Index + 1;
                     end if;
                  end;
               end if;
            end Save_Sent_String;

         begin
            if IDL.Procs.Has_Aspect (Aspects, "send_string") then
               IDL.Procs.Scan (IDL.Procs.Aspect (Aspects, "send_string"),
                               Save_Sent_String'Access);
            end if;
         end Save_Invoke_Strings;

      begin
         IDL.Procs.Scan_Commands (Proc, Save_Invoke_Strings'Access);
      end;

      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line
        ("   Init_Script : constant Script_Type :=");

      declare
         procedure Init_Local
           (Name : String;
            Typ  : IDL.Types.IDL_Type);

         ----------------
         -- Init_Local --
         ----------------

         procedure Init_Local
           (Name : String;
            Typ  : IDL.Types.IDL_Type)
         is
         begin
            if IDL.Types.Get_Name (Typ) = "page" then
               Start_Command;
               Move_Address (Registers.Element (Name), Name);
               Finish_Command;
            end if;
         end Init_Local;

      begin
         IDL.Procs.Scan_Locals (Proc, Init_Local'Access);
      end;

      declare

         procedure Put_Invoke_Command
           (Cap_Name : String;
            Endpoint : Positive;
            Aspects  : IDL.Procs.IDL_Aspects);

         ------------------------
         -- Put_Invoke_Command --
         ------------------------

         procedure Put_Invoke_Command
           (Cap_Name : String;
            Endpoint : Positive;
            Aspects  : IDL.Procs.IDL_Aspects)
         is
            Send_Count : constant Natural :=
                           IDL.Procs.Get_Length
                             (IDL.Procs.Aspect (Aspects, "send"));
            Send       : array (1 .. Send_Count) of Positive;
            Send_Index : Natural := 0;

            Recv_Count : constant Natural :=
                           IDL.Procs.Get_Length
                             (IDL.Procs.Aspect (Aspects, "recv"));
            Recv       : array (1 .. Recv_Count) of Positive;
            Recv_Index : Natural := 0;

            Have_String       : Boolean := False;
            String_List_Index : Natural := 0;
            Buffer_Register   : Natural;
            Buffer_Cap        : Natural;
            String_Register   : Natural;
            String_Index      : Natural;
            String_Length     : Natural;
            Length_Register   : Natural;

            procedure Check_Move (Arg : IDL.Procs.IDL_Expression);
            procedure Check_String (Expr : IDL.Procs.IDL_Expression);

            ----------------
            -- Check_Move --
            ----------------

            procedure Check_Move (Arg : IDL.Procs.IDL_Expression) is

            begin
               Send_Index := Send_Index + 1;
               if IDL.Procs.Is_Object_Name (Arg) then
                  declare
                     Name : constant String :=
                              IDL.Procs.Get_Object_Name (Arg);
                  begin
                     if Arg_Caps.Contains (Name) then
                        Send (Send_Index) := Arg_Caps.Element (Name);
                     elsif Locals.Contains (Name) then
                        declare
                           Typ : constant IDL.Types.IDL_Type :=
                                   Locals.Element
                                     (IDL.Procs.Get_Object_Name (Arg));
                        begin
                           if IDL.Types.Get_Name (Typ) = "page" then
                              Send (Send_Index) :=
                                Registers.Element
                                  (IDL.Procs.Get_Object_Name (Arg));
                           elsif IDL.Types.Get_Name (Typ) = "capability" then
                              Send (Send_Index) :=
                                Registers.Element
                                  (IDL.Procs.Get_Object_Name (Arg));
                           else
                              raise Constraint_Error with
                                "invalid send argument: "
                                & IDL.Procs.Get_Object_Name (Arg);
                           end if;
                        end;
                     else
                        raise Constraint_Error with
                          "undeclared: " & Name;
                     end if;
                  end;
               elsif IDL.Procs.Is_Integer_Literal (Arg) then
                  Start_Command;
                  Send (Send_Index) := Allocate_Register;
                  Move_Integer (Send (Send_Index),
                                IDL.Procs.Get_Integer_Value (Arg));
                  Finish_Command;
               elsif IDL.Procs.Is_String_Literal (Arg) then
                  raise Constraint_Error with
                    "send string with Send_String aspect";
--                    declare
--                       S : constant String :=
--                             IDL.Procs.Get_String_Value (Arg);
--                    begin
--                       Start_Command;
--                       Send (Send_Index) := Allocate_Register;
--                       Move_String (Send (Send_Index), S);
--                       Finish_Command;
--                       Send_Index := Send_Index + 1;
--                       Start_Command;
--                       Move_Integer (Send (Send_Index), S'Length);
--                       Finish_Command;
--                    end;
               end if;

            end Check_Move;

            ------------------
            -- Check_String --
            ------------------

            procedure Check_String
              (Expr : IDL.Procs.IDL_Expression)
            is
            begin
               String_List_Index := String_List_Index + 1;

               if String_List_Index = 1 then
                  Buffer_Register :=
                    Registers.Element
                      (IDL.Procs.Get_Object_Name (Expr));
               elsif String_List_Index = 2 then
                  Buffer_Cap :=
                    Registers.Element
                      (IDL.Procs.Get_Object_Name (Expr));
               elsif String_List_Index = 3 then
                  declare
                     Text : constant String :=
                              IDL.Procs.Get_String_Value
                                (IDL.Procs.Evaluate_Static_Expression
                                   (Expr));
                  begin
                     String_Register := Allocate_Register;
                     String_Index := Strs.Element (Text);
                     String_Length := Text'Length;
                  end;
               end if;
            end Check_String;

         begin

            if IDL.Procs.Has_Aspect (Aspects, "send_string") then
               declare
                  Send_String_Aspect : constant IDL.Procs.IDL_Expression :=
                                         IDL.Procs.Aspect
                                           (Aspects, "send_string");
               begin
                  Have_String := True;
                  IDL.Procs.Scan (Send_String_Aspect, Check_String'Access);
                  Start_Command;
                  Move_String (String_Register, String_Index);
                  Finish_Command;
                  Length_Register := Allocate_Register;
                  Start_Command;
                  Move_Integer (Length_Register, String_Length);
                  Finish_Command;
                  Start_Command;
                  Copy_To_Buffer
                    (Buffer_R => Buffer_Register,
                     Source_R => String_Register,
                     Length_R => Length_Register);
                  Finish_Command;
               end;
            end if;

            if IDL.Procs.Has_Aspect (Aspects, "send") then
               declare
                  Send_Aspect : constant IDL.Procs.IDL_Expression :=
                                  IDL.Procs.Aspect (Aspects, "send");
               begin
                  IDL.Procs.Scan (Send_Aspect, Check_Move'Access);
               end;
            end if;

            declare
               Recv_Aspect : constant IDL.Procs.IDL_Expression :=
                               IDL.Procs.Aspect (Aspects, "recv");

               procedure Check_Receive
                 (Expr : IDL.Procs.IDL_Expression);

               -------------------
               -- Check_Receive --
               -------------------

               procedure Check_Receive
                 (Expr : IDL.Procs.IDL_Expression)
               is
               begin
                  Recv_Index := Recv_Index + 1;
                  if IDL.Procs.Is_Object_Name (Expr) then
                     declare
                        Name : constant String :=
                                 IDL.Procs.Get_Object_Name (Expr);
                     begin
                        if Arg_Caps.Contains (Name) then
                           raise Constraint_Error with
                             "invalid receive cap: " & Name;
                        elsif Locals.Contains (Name) then
                           declare
                              Typ : constant IDL.Types.IDL_Type :=
                                      Locals.Element (Name);
                           begin
                              if IDL.Types.Get_Name (Typ) = "capability" then
                                 Recv (Recv_Index) :=
                                   Registers.Element (Name);
                              else
                                 raise Constraint_Error with
                                   "invalid receive argument: "
                                   & Name;
                              end if;
                           end;
                        else
                           raise Constraint_Error with
                             "undeclared: " & Name;
                        end if;
                     end;
                  else
                     raise Constraint_Error with
                       "invalid receive target";
                  end if;
               end Check_Receive;

            begin
               IDL.Procs.Scan (Recv_Aspect, Check_Receive'Access);
            end;

            Start_Command;
            Ada.Text_IO.Put_Line
              ("           (Command  => Invoke,");
            Ada.Text_IO.Put_Line
              ("            Cap      =>"
               & Natural'Image (Registers.Element (Cap_Name)) & ",");
            Ada.Text_IO.Put_Line
              ("            Endpoint => " & Image (Endpoint) & ",");
            Ada.Text_IO.Put_Line
              ("            Block    => "
               & (if IDL.Procs.Has_Aspect (Aspects, "block")
                   or else IDL.Procs.Has_Aspect (Aspects, "recv")
                 then "True" else "False")
               & ",");
            Ada.Text_IO.Put
              ("            Send     => (");
            if Have_String then
               Ada.Text_IO.Put (Image (Buffer_Cap) & ", "
                                & Image (Length_Register) & ", ");
            end if;
            for I in Send'Range loop
               Ada.Text_IO.Put (Image (Send (I)) & ", ");
            end loop;
            Ada.Text_IO.Put_Line ("others => 0),");
            Ada.Text_IO.Put
              ("            Recv     => (");
            for I in Recv'Range loop
               Ada.Text_IO.Put (Image (Recv (I)) & ", ");
            end loop;
            Ada.Text_IO.Put_Line
              ("others => 0),");
            Ada.Text_IO.Put_Line
              ("            Send_Arg_Count => "
               & Image (Send_Count + (if Have_String then 2 else 0))
               & ",");
            Ada.Text_IO.Put
              ("            Recv_Arg_Count => " & Image (Recv_Count) & ")");
            Finish_Command;
         end Put_Invoke_Command;

      begin
         IDL.Procs.Scan_Commands (Proc, Put_Invoke_Command'Access);
      end;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("     );");

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("end Init.Script;");

   end Generate_Init_Script;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
      Result : constant String := Integer'Image (X);
   begin
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);
      else
         return Result;
      end if;
   end Image;

   ------------------
   -- Move_Address --
   ------------------

   procedure Move_Address
     (Target_R       : Natural;
      Source_Address : String)
   is
   begin
      Ada.Text_IO.Put_Line
        ("           (Command => Move, Target_R => " & Image (Target_R)
         & ", Source_R => 0,");
      Ada.Text_IO.Put
        ("            Source_W => (Address_Value, "
         & Source_Address & "'Address))");
   end Move_Address;

   ------------------
   -- Move_Integer --
   ------------------

   procedure Move_Integer
     (Target_R : Natural;
      Value    : Integer)
   is
   begin
      Ada.Text_IO.Put_Line
        ("           (Command => Move, Target_R => " & Image (Target_R)
         & ", Source_R => 0,");
      Ada.Text_IO.Put
        ("            Source_W => (Word_Value, " & Image (Value)
         & "))");
   end Move_Integer;

   -----------------
   -- Move_String --
   -----------------

   procedure Move_String
     (Target_R : Natural;
      Value    : String)
   is
      Index : constant Natural := Strs.Element (Value);
   begin
      Ada.Text_IO.Put_Line
        ("           (Command => Move, Target_R => " & Image (Target_R)
         & ", Source_R => 0,");
      Ada.Text_IO.Put
        ("            Source_W => (String_Value, "
         & " S" & Image (Index) & "'Access"
         & "))");
   end Move_String;

   -----------------
   -- Move_String --
   -----------------

   procedure Move_String
     (Target_R : Natural;
      Index    : Natural)
   is
   begin
      Ada.Text_IO.Put_Line
        ("           (Command => Move, Target_R => " & Image (Target_R)
         & ", Source_R => 0,");
      Ada.Text_IO.Put
        ("            Source_W => (String_Value, "
         & "S" & Image (Index) & "'Access"
         & "))");
   end Move_String;

   -------------------
   -- Start_Command --
   -------------------

   procedure Start_Command is
   begin
      if Next_Command_Index = 0 then
         Ada.Text_IO.Put ("        (");
      else
         Ada.Text_IO.Put_Line (",");
         Ada.Text_IO.Put ("         ");
      end if;
      Next_Command_Index := Next_Command_Index + 1;
      Ada.Text_IO.Put_Line (Image (Next_Command_Index) & " =>");
   end Start_Command;

end IDL.Generate_Init;
