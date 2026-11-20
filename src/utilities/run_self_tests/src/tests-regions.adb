with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Region;

with Tests.Registry;

package body Tests.Regions is

   --------------------
   -- Put_Limit_Test --
   --------------------

   function Put_Limit_Test return Boolean is
      use System.Storage_Elements;
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Object_Id;

      subtype Bank_Class is Rose.Interfaces.Storage.Bank.Bank_Object'Class;
      subtype Region_Class is
        Rose.Interfaces.Storage.Region.Region_Object'Class;
      subtype Page_Data is Storage_Array (1 .. Rose.Limits.Page_Size);

      Page_Limit   : constant := 2;
      Region_Pages : constant := 4;

      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);
      --  Report Name, and fail the test, if Ok is False.

      function Filled (Value : Storage_Element) return Page_Data
      is (others => Value);

      function Put
        (Region : in out Region_Class;
         Page   : Rose.Objects.Object_Id;
         Data   : Page_Data)
         return Boolean;
      --  Put Data in Page of Region. False if the Put raised.

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Ok : Boolean) is
      begin
         if not Ok then
            Ada.Text_IO.Put (": " & Name);
            Passed := False;
         end if;
      end Check;

      ---------
      -- Put --
      ---------

      function Put
        (Region : in out Region_Class;
         Page   : Rose.Objects.Object_Id;
         Data   : Page_Data)
         return Boolean
      is
      begin
         Region.Put (Page, Data);
         return True;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return False;
      end Put;

   begin
      Ada.Text_IO.Put ("Region put limit test");

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      declare
         Test_Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                       Rose.Interfaces.Storage.Bank.Client.From_Cap
                         (Tests.Registry.Bank);
         Small     : Bank_Class :=
                       Test_Bank.Create_Sub_Bank (Page_Limit, 0);
         Region    : Region_Class :=
                       Small.Reserve_Storage
                         (Rose.Words.Word_64
                            (Region_Pages * Rose.Limits.Page_Size));
         Base      : Rose.Objects.Object_Id;
         Bound     : Rose.Objects.Object_Id;
         Data      : Page_Data;
      begin
         Region.Get_Range (Base, Bound);
         Check ("the region doesn't have four pages",
                Bound - Base = Region_Pages);

         Check ("the first page was refused",
                Put (Region, Base, Filled (1)));
         Check ("the second page was refused",
                Put (Region, Base + 1, Filled (2)));
         Check ("a page over the bank's limit was written",
                not Put (Region, Base + 2, Filled (3)));

         Region.Get (Base + 2, Data);
         Check ("the refused page doesn't read as zeroes",
                Data = Filled (0));
         Region.Get (Base + 1, Data);
         Check ("the second page lost its contents", Data = Filled (2));

         --  A page of zeroes takes no space, so writing one frees the
         --  second page's.

         Check ("a page of zeroes was refused",
                Put (Region, Base + 1, Filled (0)));
         Check ("the page was refused after space was freed",
                Put (Region, Base + 2, Filled (3)));

         Region.Get (Base + 2, Data);
         Check ("the third page lost its contents", Data = Filled (3));

         Region.Release;
         Small.Destroy;
      end;

      return Passed;
   exception
      when others =>
         Ada.Text_IO.Put (": an operation failed");
         return False;
   end Put_Limit_Test;

   ----------------
   -- Reuse_Test --
   ----------------

   function Reuse_Test return Boolean is
      use System.Storage_Elements;
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Object_Id;

      subtype Region_Class is
        Rose.Interfaces.Storage.Region.Region_Object'Class;
      subtype Page_Data is Storage_Array (1 .. Rose.Limits.Page_Size);

      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);
      --  Report Name, and fail the test, if Ok is False.

      function Filled (Value : Storage_Element) return Page_Data
      is (others => Value);

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Ok : Boolean) is
      begin
         if not Ok then
            Ada.Text_IO.Put (": " & Name);
            Passed := False;
         end if;
      end Check;

   begin
      Ada.Text_IO.Put ("Region reuse test");

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      declare
         Test_Bank  : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                        Rose.Interfaces.Storage.Bank.Client.From_Cap
                          (Tests.Registry.Bank);
         Old_Region : Region_Class :=
                        Test_Bank.Reserve_Storage (Rose.Limits.Page_Size);
         Base       : Rose.Objects.Object_Id;
         Bound      : Rose.Objects.Object_Id;
         Old_Base   : Rose.Objects.Object_Id;
         Old_Bound  : Rose.Objects.Object_Id;
         Data       : Page_Data;
         Refused    : Boolean := False;
      begin
         Old_Region.Get_Range (Old_Base, Old_Bound);
         Old_Region.Put (Old_Base, Filled (1));

         --  The released region's slot is the first free one, so the next
         --  region takes it. The cap to the old region is still in this
         --  process's table.

         Old_Region.Release;

         declare
            New_Region : Region_Class :=
                           Test_Bank.Reserve_Storage (Rose.Limits.Page_Size);
         begin
            New_Region.Get_Range (Base, Bound);
            Check ("the new region has the old region's pages",
                   Base /= Old_Base);
            New_Region.Put (Base, Filled (2));

            Old_Region.Get_Range (Old_Base, Old_Bound);
            Check ("the released region's cap has the new region's range",
                   Old_Base /= Base or else Old_Bound /= Bound);

            begin
               Old_Region.Put (Base, Filled (3));
            exception
               when Rose.Interfaces.Objects.Invocation_Error =>
                  Refused := True;
            end;
            Check ("the released region's cap wrote a page", Refused);

            Old_Region.Get (Base, Data);
            Check ("the released region's cap read the new region's page",
                   Data /= Filled (2));

            New_Region.Get (Base, Data);
            Check ("the new region's page changed", Data = Filled (2));

            New_Region.Release;
         end;
      end;

      return Passed;
   exception
      when others =>
         Ada.Text_IO.Put (": an operation failed");
         return False;
   end Reuse_Test;

end Tests.Regions;
