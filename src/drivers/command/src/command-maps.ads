package Command.Maps is

   procedure Insert
     (Command : String;
      Cap     : Rose.Capabilities.Capability);

   procedure Delete
     (Command : String);

   function Find
     (Command : String)
      return Rose.Capabilities.Capability;

end Command.Maps;
