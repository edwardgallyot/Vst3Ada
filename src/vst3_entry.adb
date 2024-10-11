with Vst3; 
with Vst3.I_Plugin_Factory_3;
with System;

package body Vst3_Entry is 
   use System;
   use Vst3.I_Plugin_Factory_3;

   function Get_Plugin_Factory return access I_Plugin_Factory_3 is 
   begin
      if Factory = null then
         Factory := new I_Plugin_Factory_3;
      -- else
         -- Factory.V_Table.Add_Ref.'Access (Factory);
      end if;
      return Factory;
   end Get_Plugin_Factory;
end Vst3_Entry;
