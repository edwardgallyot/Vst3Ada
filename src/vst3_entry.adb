with Vst3; 
with Vst3.Factory;
with System;

package body Vst3_Entry is 
   use System;
   use Vst3.Factory;

   function Get_Plugin_Factory return access Plugin_Factory is 
   begin
      if Factory = null then
         Factory := new Plugin_Factory;
      -- else
         -- Factory.V_Table.Add_Ref.'Access (Factory);
      end if;
      return Factory;
   end Get_Plugin_Factory;
end Vst3_Entry;
