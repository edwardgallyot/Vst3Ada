with Vst3; use Vst3;
with Vst3.Factory; use Vst3.Factory;
with System; use System;
with Interfaces.C; use Interfaces.C;

package body Vst3_Entry is 
   Factory : access Vst3_Factory;

   function Module_Entry (Param : Address) return C_Bool is
   begin
      return true;
   end Module_Entry;

   function Get_Plugin_Factory return System.Address is 
   begin
      if Factory = null then
         Factory := new Vst3_Factory;
      else
         declare 
            Dummy : Unsigned := Add_Ref (Factory);
         begin
            null;
         end;
      end if;
      return Factory.all'Address;
   end Get_Plugin_Factory;

   function Module_Exit (Param : Address) return C_Bool is
   begin
      return true;
   end Module_Exit;

end Vst3_Entry;
