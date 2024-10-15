with System; use System;
with Interfaces.C; use Interfaces.C;

package Vst3_Entry is 

   function Module_Entry (Param : Address) return C_Bool
      with 
         Convention => C,
         Export,
         External_Name => "ModuleEntry";

   function Get_Plugin_Factory return System.Address 
      with 
         Convention => C,
         Export,
         External_Name => "GetPluginFactory";

   function Module_Exit (Param : Address) return C_Bool
      with 
         Convention => C,
         Export,
         External_Name => "ModuleExit";

end Vst3_Entry;
