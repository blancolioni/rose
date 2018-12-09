with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

package body Configure.Writer is

   -------------------------
   -- Write_Configuration --
   -------------------------

   procedure Write_Configuration
     (Config    : Tropos.Configuration)
   is
      File : File_Type;
   begin
      Create (File, Out_File, Config.Config_Name & ".cap");
      for Caps_Config of Config.Child ("caps") loop
         declare
            Name         : constant String := Caps_Config.Config_Name;
            Cap          : array (1 .. 4) of Word_32;
            Field_Count  : constant Natural := Caps_Config.Child_Count;
            function Field (Index : Positive) return Word_32
            is (if Field_Count >= Index
                then Word_32 (Integer'(Caps_Config.Value))
                else 0);
         begin
            if Name = "create-endpoint" then
               Cap := (2, 1, 0, 0);
            elsif Name = "delete-endpoint" then
               Cap := (2, 30, 0, 0);
            elsif Name = "rescind-endpoint" then
               Cap := (2, 29, 0, 0);
            elsif Name = "register-interrupt" then
               Cap := (6, 1, Field (1), 0);
            elsif Name = "port-out-8" then
               if Field_Count = 1 then
                  Cap := (16#0000_000E#, 1, Field (1), Field (1));
               else
                  Cap := (16#0000_000E#, 3, Field (1), Field (2));
               end if;
            elsif Name = "port-in-8" then
               if Field_Count = 1 then
                  Cap := (16#0000_000E#, 2, Field (1), Field (1));
               else
                  Cap := (16#0000_000E#, 4, Field (1), Field (2));
               end if;
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "unknown cap type: " & Name);
               exit;
            end if;

            for X of Cap loop
               Write (File, X);
            end loop;

         end;

      end loop;

      Close (File);
   end Write_Configuration;

end Configure.Writer;
