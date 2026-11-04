with System;

with Rose.Words;

package Screen.Framebuffer is

   --  The framebuffer that the boot loader set up, mapped at the start of
   --  the device window.

   procedure Initialize (Success : out Boolean);
   --  Read the framebuffer's geometry from the boot loader's tag, and map
   --  it. Success is False if there is no framebuffer the driver can use:
   --  none, or one whose pixels aren't 32-bit RGB (ADR 0022).

   function Width return Rose.Words.Word;
   function Height return Rose.Words.Word;
   --  The screen's size in pixels, or zero if Initialize failed.

   procedure Map;
   --  Map the framebuffer again, after a restore: device pages are not in
   --  a checkpoint (ADR 0022, section 6).

   procedure Copy
     (Buffer  : System.Address;
      X, Y    : Rose.Words.Word;
      Columns : Rose.Words.Word;
      Rows    : Rose.Words.Word);
   --  Copy Columns by Rows pixels at X, Y from Buffer, which holds the
   --  whole screen as 32-bit XRGB pixels with no padding between rows, to
   --  the framebuffer, converting them to its format. The rectangle must
   --  be on the screen.

end Screen.Framebuffer;
