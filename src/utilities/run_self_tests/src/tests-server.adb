with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Server;

package body Tests.Server is

   use type Rose.Capabilities.Capability;
   use type Rose.Objects.Endpoint_Id;

   Object_Count : constant := 3_000;

   First_Endpoint  : constant Rose.Objects.Endpoint_Id := 16#1001#;
   Second_Endpoint : constant Rose.Objects.Endpoint_Id := 16#1002#;
   Other_Endpoint  : constant Rose.Objects.Endpoint_Id := 16#1003#;

   function Identifier
     (Index : Positive)
      return Rose.Objects.Capability_Identifier
   is (Rose.Objects.Capability_Identifier (Index));

   function Cap
     (Index    : Positive;
      Endpoint : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is (Rose.Capabilities.Capability (Index * 4)
       + (if Endpoint = First_Endpoint then 1 else 2));
   --  A cap that is not null, and that tells the object and the endpoint
   --  it was made for. It is never a real cap: this table is not asked to
   --  delete any.

   -------------------
   -- Instance_Test --
   -------------------

   function Instance_Test return Boolean is
      Table : Rose.Server.Server_Instance;
      Empty : Rose.Server.Server_Instance;
   begin
      Ada.Text_IO.Put ("Server instance test");

      for Index in 1 .. Object_Count loop
         Rose.Server.Set_Instance_Cap
           (Table, First_Endpoint, Identifier (Index),
            Cap (Index, First_Endpoint));
         Rose.Server.Set_Instance_Cap
           (Table, Second_Endpoint, Identifier (Index),
            Cap (Index, Second_Endpoint));
      end loop;

      for Index in 1 .. Object_Count loop
         if Rose.Server.Get_Instance_Cap
           (Table, First_Endpoint, Identifier (Index))
           /= Cap (Index, First_Endpoint)
           or else Rose.Server.Get_Instance_Cap
             (Table, Second_Endpoint, Identifier (Index))
           /= Cap (Index, Second_Endpoint)
           or else not Rose.Server.Has_Instance
             (Table, First_Endpoint, Identifier (Index))
           or else Rose.Server.Has_Instance
             (Table, Other_Endpoint, Identifier (Index))
         then
            return False;
         end if;
      end loop;

      --  An object nothing was set for has no caps.

      if Rose.Server.Has_Instance
           (Table, First_Endpoint, Identifier (Object_Count + 1))
        or else Rose.Server.Get_Instance_Cap
          (Table, First_Endpoint, Identifier (Object_Count + 1))
        /= Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      --  The first cap set for an endpoint is the one found.

      Rose.Server.Set_Instance_Cap
        (Table, First_Endpoint, Identifier (1), Cap (2, First_Endpoint));
      if Rose.Server.Get_Instance_Cap
        (Table, First_Endpoint, Identifier (1))
        /= Cap (1, First_Endpoint)
      then
         return False;
      end if;

      --  Removing an object takes away its caps and no one else's. The
      --  caps here are null, which Remove_Instance does not delete.

      for Index in 1 .. Object_Count loop
         Rose.Server.Set_Instance_Cap
           (Empty, First_Endpoint, Identifier (Index),
            Rose.Capabilities.Null_Capability);
      end loop;

      for Index in 1 .. Object_Count loop
         if Index mod 2 = 0 then
            Rose.Server.Remove_Instance (Empty, Identifier (Index));
         end if;
      end loop;

      for Index in 1 .. Object_Count loop
         if Rose.Server.Has_Instance
           (Empty, First_Endpoint, Identifier (Index))
           /= (Index mod 2 = 1)
         then
            return False;
         end if;
      end loop;

      return True;
   end Instance_Test;

end Tests.Server;
