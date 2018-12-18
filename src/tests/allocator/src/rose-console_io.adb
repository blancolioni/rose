with Ada.Integer_Text_IO;
with Ada.Text_IO;

package body Rose.Console_IO is

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      Ada.Text_IO.Put (Text);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Ada.Text_IO.Put_Line (Text);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X : Natural) is
   begin
      Ada.Integer_Text_IO.Put (X, 1);
   end Put;

end Rose.Console_IO;
