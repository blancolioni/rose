with Rose.Words;

package Rose.Objects is

   pragma Pure (Rose.Objects);

   type Endpoint_Id is mod 2 ** 64;

   Null_Endpoint_Id : constant Endpoint_Id := 0;

   subtype Valid_Endpoint_Id is Endpoint_Id range 1 .. Endpoint_Id'Last;

   type Endpoint_Index is mod 2 ** 16;

   type Capability_Identifier is mod 2 ** 16;

   type Rescinded_Count is mod 2 ** 24;

   type Page_Id is new Rose.Words.Word_64;
   Null_Page_Id : constant Page_Id := 0;

   type Object_Id is new Rose.Words.Word_64;
   Null_Object_Id : constant Object_Id := 0;

   subtype Page_Object_Id is Object_Id range
     16#FF00_0000_0000_0000# .. 16#FFFF_FFFF_FFFF_FFFF#;

   subtype Process_Object_Id is Object_Id range
     16#0000_0000_0000_0001# .. 16#0000_0000_FFFF_FFFF#;

   subtype Boot_Process_Object_Id is Process_Object_Id range 1 .. 255;
   --  The object ids of the boot modules.  The kernel gives idle and init
   --  the first two, and init chooses one for each boot module it
   --  launches, so that a boot server has the same object id on every
   --  boot, whichever other modules are launched.  Other processes have
   --  ids above these, which are never used again.

   Idle_Object_Id : constant Object_Id := 1;
   Init_Object_Id : constant Object_Id := 2;

   First_Process_Object_Id : constant Object_Id :=
                               Boot_Process_Object_Id'Last + 1;
   --  The object id of the first process that is not a boot module.

   function Is_Page_Object_Id (Id : Object_Id) return Boolean
   is (Id in Page_Object_Id);

   function To_Page_Id (Id : Object_Id) return Page_Id
   is (Page_Id (Id and 16#00FF_FFFF_FFFF_FFFF#));

   function Is_Process_Object_Id (Id : Object_Id) return Boolean
   is (Id in Process_Object_Id);

   type Rose_Ticks is new Rose.Words.Word_64;

   type Allocation_Count is mod 2 ** 4;
   --  a capability can be used between 1 and 15 times, or infinity times.

end Rose.Objects;
