with System.Atomic_Counters; use System.Atomic_Counters;
with Vst3; use Vst3;

package body Vst3.Plugin is 
   function Add_Ref (This : access Component) return Unsigned is 
   begin
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Component) return Unsigned is
   begin
      Decrement(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Component;  Interface_Id: TUID; Obj : access Address) return Result is 
   begin
      if Interface_Id = I_Component_IID then
         Obj.all := This.all'Address;
      end if;
      return No_Interface;
   end Query_Interface;

   function Initialise (This : access Component; Context : access Unknown) return Result is 
   begin
      return Ok_True;
   end Initialise;

   function De_Initialise (This : access Component) return Result is
   begin
      return Ok_True;
   end De_Initialise;

   function Get_Controller_Class_Id (This : access Component; Class_Id : out TUID) return Result is
   begin
      Class_Id := Make_TUID(2, 0, 0, 0);
      return Ok_True;
   end Get_Controller_Class_Id;

   function Set_Io_Mode (This : access Component; Mode : IoModes) return Result  is
   begin
      return Not_Implemented;
   end Set_Io_Mode;

   function Get_Bus_Count (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions) return Int is
   begin
      -- TODO(edg): Implement 
      return 1;      
   end Get_Bus_Count;

   function Get_Bus_Info (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; Info : access Bus_Info) return Result is
   begin
      return Ok_True;
   end Get_Bus_Info;

   function Get_Routing_Info (This : access Component; Input_Info: access Routing_Info; Output_Info : access Routing_Info) return Result is
   begin
      return Ok_True;
   end Get_Routing_Info;

   function Activate_Bus (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; State : C_Bool) return Result is
   begin
      return Ok_True;
   end Activate_Bus;

   function Set_Active (This : access Component; State : C_Bool) return Result is
   begin
      return Ok_True;
   end Set_Active;

   function Set_State (This : access Component; Stream : access Address) return Result is
   begin
      return Ok_True;
   end Set_State;

   function Get_State (This : access Component; Stream : access Address) return Result is
   begin
      return Ok_True;
   end Get_State;

end Vst3.Plugin;

