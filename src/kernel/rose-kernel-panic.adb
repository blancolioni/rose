with System.Machine_Code;
with System.Storage_Elements;

with Rose.Boot.Console;

with Rose.Kernel.Arch;

package body Rose.Kernel.Panic is

   Panic_Stack_Frame : access Rose.Kernel.Arch.Stack_Frame;
   pragma Import (C, Panic_Stack_Frame, "current_process_ptr");

   -----------
   -- Panic --
   -----------

   procedure Panic (Message : String) is
   begin
      Rose.Boot.Console.Enable_Display;
      Rose.Boot.Console.Put ("*** kernel panic: ");
      Rose.Boot.Console.Put_Line (Message);
      if False and then Panic_Stack_Frame /= null then
         Rose.Boot.Console.Put ("EAX ");
         Rose.Boot.Console.Put (Panic_Stack_Frame.EAX);
         Rose.Boot.Console.New_Line;
         Rose.Boot.Console.Put ("EIP ");
         Rose.Boot.Console.Put (Panic_Stack_Frame.EIP);
         Rose.Boot.Console.New_Line;
         Rose.Boot.Console.Put ("ESP ");
         Rose.Boot.Console.Put (Panic_Stack_Frame.ESP);
         Rose.Boot.Console.New_Line;
         Rose.Boot.Console.Put ("PSW ");
         Rose.Boot.Console.Put (Panic_Stack_Frame.PSW);
         Rose.Boot.Console.New_Line;
         declare
            Addr : constant System.Address :=
                     System'To_Address (Panic_Stack_Frame.EIP);
            Code : System.Storage_Elements.Storage_Array (1 .. 16);
            for Code'Address use Addr;
            pragma Import (Ada, Code);
         begin
            Rose.Boot.Console.Put ("Code: ");
            Rose.Boot.Console.Put (Panic_Stack_Frame.EIP);
            Rose.Boot.Console.Put (": ");
            for Element of Code loop
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put (Rose.Words.Word_8 (Element));
            end loop;
            Rose.Boot.Console.New_Line;
         end;

      end if;

      loop
         System.Machine_Code.Asm ("0:cli", Volatile => True);
         System.Machine_Code.Asm ("hlt", Volatile => True);
         System.Machine_Code.Asm ("jmp 0b", Volatile => True);
      end loop;
   end Panic;

   -----------
   -- Panic --
   -----------

   procedure Panic (Message : String;
                    Value   : Rose.Words.Word)
   is
   begin
      Rose.Boot.Console.Put (Message);
      Rose.Boot.Console.Put (Value);
      Rose.Boot.Console.New_Line;
      Panic (Message);
   end Panic;

   -----------
   -- Panic --
   -----------

   procedure Panic (Message : String;
                    Addr    : Physical_Address)
   is
   begin
      Rose.Boot.Console.Put (Message);
      Rose.Boot.Console.Put (Addr);
      Rose.Boot.Console.Put (" (physical)");
      Rose.Boot.Console.New_Line;
      loop
         null;
      end loop;
   end Panic;

   -----------
   -- Panic --
   -----------

   procedure Panic (Message : String;
                    Addr    : Virtual_Address)
   is
   begin
      Rose.Boot.Console.Put (Message);
      Rose.Boot.Console.Put (Physical_Address (Addr));
      Rose.Boot.Console.Put (" (virtual)");
      Rose.Boot.Console.New_Line;
      loop
         null;
      end loop;
   end Panic;

end Rose.Kernel.Panic;
