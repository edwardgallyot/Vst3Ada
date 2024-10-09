with Vst3; use Vst3;

package body Vst3_Entry is 
   function Get_Plugin_Factory return Integer is 
   begin
      Say_Hi;
      return 1024 * 4096;
   end Get_Plugin_Factory;
end Vst3_Entry;
