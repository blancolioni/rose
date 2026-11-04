package Tests.Shared_Regions is

   function Shared_Regions_Test return Boolean;
   --  Rose.Shared_Regions (ADR 0025): map one region twice, and check
   --  that the two mappings don't overlap and share their pages, that an
   --  unmapped address is used again, and that unmapping an address that
   --  isn't mapped raises Mapping_Error. Skipped without a bank.

end Tests.Shared_Regions;
