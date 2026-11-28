with Rose.System_Calls;

with Virtio_Net.Server;

procedure Virtio_Net.Driver is
begin
   Rose.System_Calls.Use_Capabilities
     (Create_Endpoint => Create_Endpoint_Cap);
   Virtio_Net.Server.Create_Server;
   Virtio_Net.Server.Start_Server;
end Virtio_Net.Driver;
