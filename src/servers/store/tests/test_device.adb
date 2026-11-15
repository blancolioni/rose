with Test_Space;

package body Test_Device is

   type Page_Array is array (Device_Page range 1 .. Page_Count) of Page_Words;
   type Image_Array is array (0 .. 1, 1 .. 1) of Page_Words;

   Pages  : Page_Array := (others => (others => 0));
   Images : Image_Array := (others => (others => (others => 0)));

   Bad_Write_Count : Natural := 0;
   Bad_Read_Count  : Natural := 0;
   Write_Count     : Natural := 0;

   function Bad_Reads return Natural is (Bad_Read_Count);
   function Bad_Writes return Natural is (Bad_Write_Count);
   function Writes return Natural is (Write_Count);

   procedure Peek
     (Page : Device_Page;
      Data : out Page_Words)
   is
   begin
      if Page in Pages'Range then
         Data := Pages (Page);
      else
         Bad_Read_Count := Bad_Read_Count + 1;
         Data := (others => 0);
      end if;
   end Peek;

   procedure Read
     (Page : Device_Page;
      Data : out Page_Words)
   is
   begin
      Peek (Page, Data);
   end Read;

   procedure Read_Image
     (Copy  : Natural;
      Index : Positive;
      Data  : out Page_Words)
   is
   begin
      Data := Images (Copy, Index);
   end Read_Image;

   procedure Write
     (Page : Device_Page;
      Data : Page_Words)
   is
   begin
      Write_Count := Write_Count + 1;
      if Page not in Pages'Range or else not Test_Space.Is_Born (Page) then
         Bad_Write_Count := Bad_Write_Count + 1;
         return;
      end if;
      Pages (Page) := Data;
   end Write;

   procedure Write_Image
     (Copy  : Natural;
      Index : Positive;
      Data  : Page_Words)
   is
   begin
      Images (Copy, Index) := Data;
   end Write_Image;

end Test_Device;
