with Vst3.Component; use Vst3.Component;
with Vst3.Controller; use Vst3.Controller;
with Vst3.Processor; use Vst3.Processor;

package Vst3.Plugin is 
   procedure Dummy;

   type Vst3_Plugin is record
      Component   : aliased Vst3_Component;
      Controller  : aliased Vst3_Controller;
      Processor   : aliased Vst3_Processor;
   end record 
   with Convention => C_Pass_By_Copy;

end Vst3.Plugin;
