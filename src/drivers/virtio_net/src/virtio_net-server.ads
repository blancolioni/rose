package Virtio_Net.Server is

   procedure Create_Server;
   --  Find the card, start it, and publish Network_Device. Without a
   --  card, only Restorable is attached, and the driver serves nothing.

   procedure Start_Server;

end Virtio_Net.Server;
