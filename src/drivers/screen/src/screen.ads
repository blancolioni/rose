with Rose.Capabilities;

package Screen with Pure is

   --  The screen driver (ADR 0022, #582). It holds the boot loader's
   --  linear framebuffer, and gives the whole screen out as one surface,
   --  whose buffer is ordinary memory that it shares with the surface's
   --  holder (ADR 0025).

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Framebuffer_Cap     : constant Rose.Capabilities.Capability := 3;
   --  Maps the framebuffer in the device window, at the address sent.
   Geometry_Cap        : constant Rose.Capabilities.Capability := 4;
   --  Reads the boot loader's framebuffer tag (multiboot2 tag 8).
   Bank_Cap            : constant Rose.Capabilities.Capability := 5;
   --  The bank that the surface's buffer is reserved from.

end Screen;
