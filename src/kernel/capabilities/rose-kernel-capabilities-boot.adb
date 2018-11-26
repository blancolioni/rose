with Rose.Words;

with Rose.Kernel.Modules;
with Rose.Kernel.Processes.Init;
with Rose.Kernel.Validation;

with Rose.Boot.Console;

with Rose.Environment_Pages;

package body Rose.Kernel.Capabilities.Boot is

   Page : aliased Rose.Environment_Pages.Environment_Page;

   procedure Create_Environment (Params : Rose.Invocation.Invocation_Access);

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment
     (Params : Rose.Invocation.Invocation_Access)
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
      Set_Value (Argument_Count);
      Set_Environment_Value (Page, "@arg-count", Value (1 .. Length));
      Rose.Boot.Console.Put ("@arg-count = ");
      Rose.Boot.Console.Put (Value (1 .. Length));
      Rose.Boot.Console.New_Line;

      for I in 1 .. Argument_Count loop
         declare
            Name : String := "@arg00";
         begin
            Name (Name'Last) := Character'Val (I mod 10 + 48);
            Name (Name'Last - 1) := Character'Val (I / 10 mod 10 + 48);
            Set_Value (Params.Data (Argument_Index));
            Set_Environment_Value (Page, Name, Value (1 .. Length));
            Rose.Boot.Console.Put (Name);
            Rose.Boot.Console.Put (" = ");
            Rose.Boot.Console.Put (Value (1 .. Length));
            Rose.Boot.Console.New_Line;
            Argument_Index := Argument_Index + 1;
         end;
      end loop;
   end Create_Environment;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      pragma Unreferenced (Cap);
      use Rose.Invocation;
      Process_Id : Rose.Objects.Process_Id;

   begin

      Create_Environment (Params);

      Process_Id :=
        Rose.Kernel.Processes.Init.Load_Boot_Module
          (Priority => Rose.Kernel.Processes.Process_Priority
             (Params.Data (0)),
           Module   =>
             Rose.Kernel.Modules.Module_Index
               (Params.Data (0)),
           Environment => Page'Access);

      Rose.Boot.Console.Put ("boot module ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Data (0)));

      if Params.Control.Flags (Send_Caps) then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            declare
               To_Cap : constant Rose.Capabilities.Capability :=
                          Rose.Kernel.Processes.Create_Cap
                            (Process_Id);
            begin
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (Params.Caps (Cap_Index)));
               Rose.Boot.Console.Put ("/");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (To_Cap));

               Rose.Kernel.Processes.Copy_Cap_Layout
                 (From_Process => Rose.Kernel.Processes.Current_Process_Id,
                  From_Cap     => Params.Caps (Cap_Index),
                  To_Process   => Process_Id,
                  To_Cap       => To_Cap);
               Rose.Kernel.Validation.Create_Cap
                 (Process_Id, To_Cap,
                  Rose.Kernel.Processes.Cap_Type
                    (Process_Id, To_Cap));
            end;
         end loop;
      end if;

      Rose.Boot.Console.New_Line;

      Params.Control.Flags := (Reply  => True, Send_Words => True,
                               others => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Data (0) := Rose.Words.Word (Process_Id);
      Rose.Kernel.Processes.Set_Current_State
        (Rose.Kernel.Processes.Current_Process_Id,
         Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Boot;
