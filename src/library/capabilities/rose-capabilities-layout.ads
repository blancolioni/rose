with Ada.Unchecked_Conversion;

with Rose.Objects;

package Rose.Capabilities.Layout is

   Capability_Header_Bytes : constant := 8;
   Capability_Layout_Bytes : constant := 16;

   type Cap_Identifier is mod 2 ** 20 with Size => 20;

   type Capability_Type is
     (Null_Cap,
      Page_Object_Cap,
      Meta_Cap,
      Process_Cap,
      Endpoint_Cap,
      Receive_Cap,
      Cap_Set,
      Kernel_Cap,
      Boot_Cap,
      Copy_Cap,
      Physical_Memory_Cap,
      Create_Cap,
      Page_Table_Cap,
      Reply_Cap,
      Arch_Cap,
      Other_Cap)
     with Size => 4;

   type Generic_Capability_Flag is range 1 .. 4;

   type Generic_Capability_Flags is
     array (Generic_Capability_Flag) of Boolean with Pack;

   type Generic_Capability_Header is
      record
         Cap_Type    : Capability_Type               := Null_Cap;
         Flags       : Generic_Capability_Flags := (others => False);
         Alloc_Count : Rose.Objects.Allocation_Count := 0;
         Identifier  : Cap_Identifier                := 0;
         Endpoint    : Rose.Objects.Endpoint_Index   := 0;
      end record
     with Pack, Size => 64;

   type Generic_Capability_Layout is
      record
         Header   : Generic_Capability_Header;
         Payload  : Rose.Objects.Object_Id;
      end record
        with Pack, Size => 128;

   Empty_Capability : constant Generic_Capability_Layout := (others => <>);

   function Reply_Capability
     (Pid : Rose.Objects.Process_Id)
      return Generic_Capability_Layout
   is ((Reply_Cap, Alloc_Count => 1, others => <>),
       Rose.Objects.To_Object_Id (Pid));

   function Receive_Capability
     (Pid : Rose.Objects.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Generic_Capability_Layout;

   function Endpoint_Capability
     (Pid      : Rose.Objects.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Generic_Capability_Layout;

   type Capability_Object_Payload is
      record
         Object_Type  : Capability_Type;
         X, Y         : Boolean;
         Short_Header : Rose.Words.Word_8;
         Long_Header  : Rose.Words.Word_16;
         Payload      : Rose.Words.Word_32;
      end record
     with Pack, Size => 64;

   type Process_Object_Payload is
      record
         Object_Type  : Capability_Type;
         X, Y         : Boolean;
         Short_Header : Rose.Words.Word_8;
         Long_Header  : Rose.Words.Word_16;
         Process_Id   : Rose.Objects.Process_Id;
      end record
     with Pack, Size => 64;

   type Endpoint_Object_Payload is
      record
         Object_Type  : Capability_Type;
         X, Y         : Boolean;
         Endpoint     : Rose.Words.Word_8;
         Long_Header  : Rose.Words.Word_16;
         Process_Id   : Rose.Objects.Process_Id;
      end record
     with Pack, Size => 64;

   type Page_Object_Payload is
      record
         Page_Object : Rose.Objects.Page_Object_Id;
      end record
     with Pack, Size => 64;

   type Endpoint_Capability_Layout is
      record
         Header   : Generic_Capability_Header;
         Endpoint : Endpoint_Object_Payload;
      end record
     with Pack, Size => 128;

   function To_Generic
   is new Ada.Unchecked_Conversion
     (Endpoint_Capability_Layout, Generic_Capability_Layout);

   type Process_Capability_Layout is
      record
         Header  : Generic_Capability_Header;
         Process : Process_Object_Payload;
      end record
     with Pack, Size => 128;

   function To_Generic
   is new Ada.Unchecked_Conversion
     (Process_Capability_Layout, Generic_Capability_Layout);

   function To_Process
   is new Ada.Unchecked_Conversion
     (Generic_Capability_Layout, Process_Capability_Layout);

   function Is_Process_Cap
     (Cap : Generic_Capability_Layout)
      return Boolean
   is (Cap.Header.Cap_Type = (Process_Cap));

   function Make_Process_Cap
     (Pid            : Rose.Objects.Process_Id;
      Identity       : Cap_Identifier;
      Alloc_Count    : Rose.Objects.Allocation_Count := 0)
      return Generic_Capability_Layout
   is (To_Generic
       (Process_Capability_Layout'
          (Header   => Generic_Capability_Header'
             (Cap_Type          => Process_Cap,
              Alloc_Count       => Alloc_Count,
              Identifier        => Identity,
              others            => <>),
           Process => Process_Object_Payload'
             (Object_Type  => Endpoint_Cap,
              X            => False,
              Y            => False,
              Short_Header => 0,
              Long_Header  => 0,
              Process_Id   => Pid))));

   function Get_Process_Id
     (Cap : Generic_Capability_Layout)
      return Rose.Objects.Process_Id
   is (To_Process (Cap).Process.Process_Id);

   function Make_Endpoint_Cap
     (Pid            : Rose.Objects.Process_Id;
      Endpoint       : Rose.Objects.Endpoint_Index;
      Identity       : Cap_Identifier;
      Alloc_Count    : Rose.Objects.Allocation_Count := 0)
      return Generic_Capability_Layout
     is (Header => Generic_Capability_Header'
           (Cap_Type          => Endpoint_Cap,
            Flags             => <>,
            Alloc_Count       => Alloc_Count,
            Identifier        => Identity,
            Endpoint          => Endpoint),
         Payload => Rose.Objects.To_Object_Id (Pid));

   type Page_Object_Capability_Layout is
      record
         Header : Generic_Capability_Header;
         Page   : Page_Object_Payload;
      end record
     with Pack, Size => 128;

   function To_Generic
   is new Ada.Unchecked_Conversion
     (Page_Object_Capability_Layout, Generic_Capability_Layout);

   function To_Page_Object
   is new Ada.Unchecked_Conversion
     (Generic_Capability_Layout, Page_Object_Capability_Layout);

   function Make_Page_Object_Cap
     (Readable         : Boolean;
      Writable         : Boolean;
      To_Shared_Buffer : Boolean;
      Page_Object      : Rose.Objects.Page_Object_Id;
      Alloc_Count      : Rose.Objects.Allocation_Count := 0)
      return Generic_Capability_Layout
   is (To_Generic
       (Page_Object_Capability_Layout'
          (Header   => Generic_Capability_Header'
             (Cap_Type          => Page_Object_Cap,
              Flags             =>
                (False, To_Shared_Buffer, Writable, Readable),
              Alloc_Count       => Alloc_Count,
              Identifier        => 0,
              Endpoint          => 0),
           Page     => Page_Object_Payload'
             (Page_Object => Page_Object))));

   function Get_Page
     (Cap : Generic_Capability_Layout)
      return Rose.Objects.Page_Object_Id
   is (To_Page_Object (Cap).Page.Page_Object);

   type Cap_Set_Object_Payload is
     array (Capability range 1 .. 64) of Boolean
     with Pack, Size => 64;

   type Cap_Set_Capability_Layout is
      record
         Header  : Generic_Capability_Header;
         Set     : Cap_Set_Object_Payload;
      end record
     with Pack, Size => 128;

   function To_Generic
   is new Ada.Unchecked_Conversion
     (Cap_Set_Capability_Layout, Generic_Capability_Layout);

   function To_Cap_Set
   is new Ada.Unchecked_Conversion
     (Generic_Capability_Layout, Cap_Set_Capability_Layout);

   function Make_Cap_Set
     (Identity       : Cap_Identifier)
      return Cap_Set_Capability_Layout
   is (Cap_Set_Capability_Layout'
         (Header   => Generic_Capability_Header'
            (Cap_Type          => Cap_Set,
             Identifier        => Identity,
             others            => <>),
          Set      => (others => False)));

end Rose.Capabilities.Layout;
