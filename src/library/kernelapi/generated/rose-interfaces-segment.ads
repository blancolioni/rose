with Rose.Objects;

package Rose.Interfaces.Segment is

   Segment_Readable : constant  := 1;
   Segment_Writable : constant  := 2;
   Segment_Executable : constant  := 4;
   Segment_Resizable : constant  := 8;
   Segment_Initialized : constant  := 16;
   Segment_Interface : constant Rose.Objects.Endpoint_Id :=
      16#25CE_5F3A_F35C#;
   Add_Segment_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#FFBB_6E9C_85B9#;
   Add_Nonpersistent_Segment_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#7C63_63AC_C85C#;

end Rose.Interfaces.Segment;
