with Rose.Objects;
with Rose.Words;

with Rose.Kernel.Modules;
with Rose.Kernel.Processes.Init;

with Rose.Boot.Console;

with Rose.Environment_Pages;

package body Rose.Kernel.Capabilities.Boot is

   Page : aliased Rose.Environment_Pages.Environment_Page;

   function Check_Environment
     (Params : Rose.Invocation.Invocation_Access)
      return Boolean;
   --  Return True if environment was created

   ------------------------
   -- Create_Environment --
   ------------------------

   function Check_Environment
     (Params : Rose.Invocation.Invocation_Access)
      return Boolean
   is
      use Rose.Invocation;
      use Rose.Words;
      use Rose.Environment_Pages;
      Start : constant Parameter_Word_Index := 3;

      Value : String (1 .. 12);
      Length : Natural;

      procedure Set_Value (X : Word);

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (X : Word) is
         It : Word := X;
      begin
         if X = 0 then
            Length := 1;
            Value (Length) := '0';
            return;
         end if;

         Length := 0;
         while It > 0 loop
            Length := Length + 1;
            Value (Length) := Character'Val (It mod 10 + 48);
            It := It / 10;
         end loop;

         for I in 1 .. Length / 2 loop
            declare
               Ch : constant Character := Value (I);
            begin
               Value (I) := Value (Length - I + 1);
               Value (Length - I + 1) := Ch;
            end;
         end loop;
      end Set_Value;

      Argument_Count : constant Word :=
                         Word (Params.Control.Last_Sent_Word) - 2;
      Argument_Index : Parameter_Word_Index := Start;
   begin
      if Argument_Count = 0 then
         return False;
      end if;

      Set_Value (Argument_Count);
      Set_Environment_Value (Page, "@arg-count", Value (1 .. Length));

      for I in 1 .. Argument_Count loop
         declare
            Name : String := "@arg00";
         begin
            Name (Name'Last) := Character'Val (I mod 10 + 48);
            Name (Name'Last - 1) := Character'Val (I / 10 mod 10 + 48);
            Set_Value (Params.Data (Argument_Index));
            Set_Environment_Value (Page, Name, Value (1 .. Length));
            Argument_Index := Argument_Index + 1;
         end;
      end loop;
      return True;
   end Check_Environment;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      pragma Unreferenced (Cap);
      use Rose.Invocation;
      Current_Pid      : constant Rose.Kernel.Processes.Process_Id :=
                           Rose.Kernel.Processes.Current_Process_Id;
      New_Pid          : Rose.Kernel.Processes.Process_Id;
      Oid              : constant Rose.Objects.Object_Id :=
                           Rose.Objects.Object_Id (Params.Data (2));
      Have_Environment : Boolean;

   begin

      --  Data: the module, its priority, its object id, and its
      --  arguments.  The object id is one of the boot modules', other
      --  than idle's and init's, and no live process has it.

      declare
         use Rose.Kernel.Processes;
         use type Rose.Objects.Object_Id;
      begin
         if Params.Control.Last_Sent_Word < 2
           or else Oid not in Rose.Objects.Boot_Process_Object_Id
           or else Oid <= Rose.Objects.Init_Object_Id
           or else To_Process_Id (Oid) /= Null_Process_Id
         then
            Return_Error (Params, Request_Error);
            return;
         end if;
      end;

      Have_Environment := Check_Environment (Params);

      New_Pid :=
        Rose.Kernel.Processes.Init.Load_Boot_Module
          (Priority => Rose.Kernel.Processes.Process_Priority
             (Params.Data (1)),
           Module   =>
             Rose.Kernel.Modules.Module_Index
               (Params.Data (0)),
           Oid         => Oid,
           Environment => (if Have_Environment
                           then Page'Access
                             else null));

      Rose.Boot.Console.Put ("boot module ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Data (0)));

      if Params.Control.Flags (Send_Caps) then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            declare
               To_Cap : constant Rose.Capabilities.Capability :=
                          Rose.Kernel.Processes.Create_Cap
                            (New_Pid);
            begin
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (To_Cap));
               Rose.Boot.Console.Put ("/");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (Params.Caps (Cap_Index)));

               Rose.Kernel.Processes.Copy_Cap_Layout
                 (From_Process_Id => Current_Pid,
                  From_Cap        => Params.Caps (Cap_Index),
                  To_Process_Id   => New_Pid,
                  To_Cap          => To_Cap);
            end;
         end loop;
      end if;

      Rose.Boot.Console.New_Line;

      --  If the caller receives caps, the reply has a copy cap for the
      --  new process, so that the caller never has to make one (#115).

      declare
         use type Rose.Capabilities.Capability;
         Receives_Caps : constant Boolean :=
                           Params.Control.Flags (Recv_Caps);
         Copy_Cap      : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
      begin
         if Receives_Caps then
            Copy_Cap := Rose.Kernel.Processes.Create_Cap (Current_Pid);
            if Copy_Cap /= Rose.Capabilities.Null_Capability then
               Rose.Kernel.Processes.Set_Cap
                 (Pid    => Current_Pid,
                  Cap    => Copy_Cap,
                  Layout =>
                    (Header  =>
                       (Cap_Type => Rose.Capabilities.Layout.Copy_Cap,
                        Endpoint => 1,
                        others   => <>),
                     Payload => Oid));
            end if;
         end if;

         Params.Control.Flags := (Reply  => True, others => False);

         if Copy_Cap /= Rose.Capabilities.Null_Capability then
            Params.Control.Flags (Send_Caps) := True;
            Params.Control.Last_Sent_Cap := 0;
            Params.Caps (0) := Copy_Cap;
         end if;
      end;

      Rose.Invocation.Send_Object_Id
        (Params.all, Rose.Kernel.Processes.To_Object_Id (New_Pid));
      Rose.Kernel.Processes.Set_Current_State
        (Current_Pid,
         Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Boot;
