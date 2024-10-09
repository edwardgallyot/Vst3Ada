
package Vst3_Entry is 
   function Get_Plugin_Factory return Integer 
      with 
         Convention => C,
         Export,
         External_Name => "GetPluginFactory";
end Vst3_Entry;
