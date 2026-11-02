with Rose.Words;                        use Rose.Words;

package Rose.Multiboot is

   procedure Load_Multiboot_Header;

   procedure Check_Magic;

   function Have_Physical_Memory_Range return Boolean;
   function Have_Memory_Map return Boolean;
   function Have_Modules return Boolean;

   function Physical_Memory_Low return Word_32;
   function Physical_Memory_High return Word_32;

   function Have_Framebuffer return Boolean;
   --  True if the boot loader gave us a linear framebuffer that we can
   --  use (#582): 32-bit RGB, ending below 4 GB.  A text mode, which
   --  GRUB also reports with a framebuffer tag, is not one.

   function Framebuffer_Address return Word_32
     with Pre => Have_Framebuffer;
   function Framebuffer_Pitch return Word_32
     with Pre => Have_Framebuffer;
   --  Bytes from the start of one row to the start of the next
   function Framebuffer_Width return Word_32
     with Pre => Have_Framebuffer;
   function Framebuffer_Height return Word_32
     with Pre => Have_Framebuffer;

   procedure Scan_Modules
     (Process : not null access
        procedure (Mod_Start  : Word_32;
                   Mod_End    : Word_32;
                   Mod_Text   : String));

   procedure Scan_Memory_Map
     (Process : not null access
        procedure (Available : Boolean;
                   Low       : Word_64;
                   High      : Word_64));

   procedure Scan_Kernel_Arguments
     (Process : not null access
        procedure (Argument : String));

private

   Magic_Value : constant := 16#36D7_6289#;

   --  We need to import the "mbd" symbol...
   Multiboot_Address : constant Word_32;

   pragma Import (Assembly, Multiboot_Address, "mbd");

end Rose.Multiboot;
