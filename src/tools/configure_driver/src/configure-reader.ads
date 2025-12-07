with Configure.Caps;

package Configure.Reader is

   function Read
     (Path : String)
      return Configure.Caps.Cap_Config_List;

end Configure.Reader;
