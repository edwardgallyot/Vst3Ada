with System;
with Vst3; 
with Vst3.Factory;  use Vst3.Factory;

package Vst3_Entry is 
   use System;
   Factory: access Plugin_Factory;

   function Get_Plugin_Factory return access Plugin_Factory
      with 
         Global => (Factory),
         Convention => C,
         Export,
         External_Name => "GetPluginFactory";
end Vst3_Entry;
