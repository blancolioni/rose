package Tests.Shared_Segments is

   function Shared_Segment_Test return Boolean;
   --  Map one region at two addresses as shared segments, and once more
   --  as a private one, and check that the shared mappings see each
   --  other's writes, that the private one doesn't, and that the region
   --  has the writes once nothing maps it (ADR 0025). Skipped without a
   --  bank.

end Tests.Shared_Segments;
