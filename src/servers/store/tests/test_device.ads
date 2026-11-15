with Store.Device_Pages;

package Test_Device is

   --  A backing store in memory, of pages 1 .. Page_Count, and the two
   --  copies of the free map's image, which on a real device are its
   --  first pages.

   use Store.Device_Pages;

   Page_Count : constant := 8191;

   procedure Read
     (Page : Device_Page;
      Data : out Page_Words);

   procedure Write
     (Page : Device_Page;
      Data : Page_Words);
   --  Every write must be to a page that is born: one allocated since
   --  the last checkpoint, which no checkpoint names.  Any other write
   --  is counted in Bad_Writes.

   procedure Peek
     (Page : Device_Page;
      Data : out Page_Words);
   --  Read, without counting it.

   procedure Read_Image
     (Copy  : Natural;
      Index : Positive;
      Data  : out Page_Words);

   procedure Write_Image
     (Copy  : Natural;
      Index : Positive;
      Data  : Page_Words);

   function Bad_Writes return Natural;
   --  Writes to a page that isn't born, or isn't on the device.

   function Bad_Reads return Natural;
   --  Reads of a page that isn't on the device.

   function Writes return Natural;

end Test_Device;
