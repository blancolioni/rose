with Rose.Containers.Queues;
with Rose.Words;

package Rose.Containers.Queues_Proof
  with SPARK_Mode
is

   --  gnatprove proves a generic through its instances: this is the one
   --  that isofs uses.

   package Word_Queues is
     new Rose.Containers.Queues
       (Rose.Words.Word_32,
        Rose.Words.Word_32,
        Rose.Words."<",
        Rose.Words."=");

end Rose.Containers.Queues_Proof;
