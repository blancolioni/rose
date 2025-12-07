with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

package body Configure.Writer is

   -------------------------
   -- Write_Configuration --
   -------------------------

   procedure Write_Configuration
     (Driver_Name : String;
      Caps        : Configure.Caps.Cap_Config_List)
   is
      File : File_Type;

      procedure Add_Cap (Cap_Config : Configure.Caps.Cap_Config);

      -------------
      -- Add_Cap --
      -------------

      procedure Add_Cap (Cap_Config : Configure.Caps.Cap_Config) is
         Name         : constant String := Cap_Config.Name;
         Cap          : array (1 .. 4) of Word_32;
         Field_Count  : constant Natural := Cap_Config.Field_Count;
         function Field (Index : Positive) return Word_32
         is (if Index <= Field_Count
             then Word_32 (Integer'(Cap_Config.Field (Index)))
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
            return;
         end if;

         for X of Cap loop
            Write (File, X);
         end loop;

      end Add_Cap;

   begin
      Create (File, Out_File, Driver_Name & ".cap");
      Caps.Iterate (Add_Cap'Access);
      Close (File);
   end Write_Configuration;

end Configure.Writer;
