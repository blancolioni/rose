with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Server;

with Rose.Interfaces.Processes.Restorable.Server;

package body Transient_Holder is

   Shifted : constant String := "sptf.635.usbotjfou.nbslfs";
   --  The marker, each character one more than it is, so that the marker
   --  itself is not in the program's image, which the store keeps on the
   --  disk. Scripts/test-boot looks for the marker on the disk.

   Held_Pages : constant := 16;
   Page_Size  : constant := 4096;

   Held : String (1 .. Held_Pages * Page_Size)
     with Volatile;
   --  The marker, again and again, in the process's data.

   Context : Rose.Server.Server_Context;

   type Holder_Object is
     limited new Rose.Interfaces.Processes.Restorable.Server
       .Restorable_Interface
   with null record;

   overriding function Restorable_Cap
     (Item : in out Holder_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   overriding procedure Restored (Item : in out Holder_Object);

   The_Holder : aliased Holder_Object;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      Marker : String (Shifted'Range);
   begin
      for I in Shifted'Range loop
         Marker (I) := Character'Val (Character'Pos (Shifted (I)) - 1);
      end loop;

      for I in Held'Range loop
         Held (I) :=
           Marker (Marker'First + (I - Held'First) mod Marker'Length);
      end loop;

      Rose.Interfaces.Processes.Restorable.Server.Attach
        (Context, The_Holder'Access);
      Ada.Text_IO.Put_Line
        ("hold_transient: holding its marker in"
         & Natural'Image (Held_Pages) & " pages");
   end Create_Server;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Holder_Object) is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line
        ("hold_transient: FAIL: restored, but it is transient");
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Transient_Holder;
