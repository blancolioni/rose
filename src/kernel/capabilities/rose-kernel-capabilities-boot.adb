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
      Start : constant Parameter_Word_Index := 2;

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
                         Word (Params.Control.Last_Sent_Word) - 1;
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
      Have_Environment : constant Boolean :=
                           Check_Environment (Params);

   begin

      New_Pid :=
        Rose.Kernel.Processes.Init.Load_Boot_Module
          (Priority => Rose.Kernel.Processes.Process_Priority
             (Params.Data (1)),
           Module   =>
             Rose.Kernel.Modules.Module_Index
               (Params.Data (0)),
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

      Params.Control.Flags := (Reply  => True, others => False);
      Rose.Invocation.Send_Object_Id
        (Params.all, Rose.Kernel.Processes.To_Object_Id (New_Pid));
      Rose.Kernel.Processes.Set_Current_State
        (Current_Pid,
         Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Boot;
