with System;
with Vst3; 
with Vst3.I_Plugin_Factory_3;  use Vst3.I_Plugin_Factory_3;

package Vst3_Entry is 
   use System;
   Factory: access I_Plugin_Factory_3;

   function Get_Plugin_Factory return access I_Plugin_Factory_3
      with 
         Global => (Factory),
         Convention => C,
         Export,
         External_Name => "GetPluginFactory";
end Vst3_Entry;
