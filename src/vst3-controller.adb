
with System.Atomic_Counters; use System.Atomic_Counters;

package body Vst3.Controller is 
   function Add_Ref (This : access Vst3_Controller) return Unsigned is 
   begin
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_Controller) return Unsigned is
   begin
      Decrement(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Vst3_Controller;  Interface_Id: TUID; Obj : access Address) return Result is 
   begin
      if Interface_Id = I_Controller_IID then
         Obj.all := This.all'Address;
      end if;
      return No_Interface;
   end Query_Interface;

   function Initialise (This : access Vst3_Controller; Context : access F_Unknown) return Result is 
   begin
      return Ok_True;
   end Initialise;

   function De_Initialise (This : access Vst3_Controller) return Result is
   begin
      return Ok_True;
   end De_Initialise;

   function Set_Component_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      return Ok_True;
   end Set_Component_State;

   function Set_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      return Ok_True;
   end Set_State;

   function Get_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      return Ok_True;
   end Get_State;

   function Get_Parameter_Count (This : access Vst3_Controller) return Int is
   begin
      return 0;
   end Get_Parameter_Count;

   function Get_Parameter_Info (This : access Vst3_Controller; Index : Int; Info : access Steinberg_Vst_ParameterInfo) return Result is
   begin
      return Ok_True;
   end Get_Parameter_Info;

end Vst3.Controller;
