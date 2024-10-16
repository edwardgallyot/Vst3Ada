with System.Atomic_Counters; use System.Atomic_Counters;
with Vst3.Constants; use Vst3.Constants;
with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;

package body Vst3.Controller is 
   function Add_Ref (This : access Vst3_Controller) return Unsigned is 
   begin
      Vst3_Log("Called Vst3.Controller.Add_Ref");
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_Controller) return Unsigned is
   begin
      Vst3_Log("Called Vst3.Controller.Release");
      Decrement(This.Ref_Count);
      -- TODO(edg): We need to deallocate the memory here but not for now.
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Vst3_Controller;  Interface_Id: TUID; Obj : access Address) return Result is 
   begin
      Vst3_Log("Called Vst3.Controller.Query_Interface");
      if Interface_Id = Controller_Id  or 
         Interface_Id = Unknown_Id     or
         Interface_Id = Plugin_Base_Id 
      then
         Obj.all := This.all'Address;
         -- TODO(edg): We should have a look at MIDI mapping here
         return Ok_True;
      end if;
      return No_Interface;
   end Query_Interface;

   function Initialise (This : access Vst3_Controller; Context : access F_Unknown) return Result is 
   begin
      Vst3_Log("Called Vst3.Controller.Initialise");
      return Ok_True;
   end Initialise;

   function De_Initialise (This : access Vst3_Controller) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.De_Initialise");
      return Ok_True;
   end De_Initialise;

   function Set_Component_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Set_Component_State");
      return Not_Implemented;
   end Set_Component_State;

   function Set_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Set_State");
      return Not_Implemented;
   end Set_State;

   function Get_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_State");
      return Not_Implemented;
   end Get_State;

   function Get_Parameter_Count (This : access Vst3_Controller) return Int is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Parameter_Count");
      -- TODO(edg): How do we want to configure parameters
      return 1;
   end Get_Parameter_Count;

   function Get_Parameter_Info (This : access Vst3_Controller; Index : Int; Info : access Parameter_Info) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Parameter_Info for Index: " & Index'Image & "and Info: " & Info'Image);
      Info.all := (
         Id => Param_Id(Index),
         Title => To_C("Test M8"),
         Short_Title => To_C("Test"),
         Units => To_C("Dingles"),
         Step_Count => 20,
         Default_Normalised_Value => 0.0,
         Unit_Id => 0,
         Flags => Int(Parameter_Flags(CanAutomate))
      );
      return Ok_True;
   end Get_Parameter_Info;

   function Get_Param_String_By_Value (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value; Display : access C_Wide_String_128) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_String_By_Value");

      -- TODO(edg): Are we going to index via the param id?
      Display.all := To_C ((Trim (Value'Wide_Image, Ada.Strings.Both) & " Dingles"));
      return Ok_True;
   end Get_Param_String_By_Value;

   function Get_Param_Value_By_String (This : access Vst3_Controller; Id : Param_Id; String : access Wide_Character; Value : Param_Value) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_Value_By_String");
      return Ok_True;
   end Get_Param_Value_By_String;

   function Normalised_Param_To_Plain (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Param_Value  is
   begin
      Vst3_Log("Called Vst3.Controller.Normalised_Param_To_Plain");
      return 0.0;
   end Normalised_Param_To_Plain;

   function Plain_Param_To_Normalised (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Param_Value  is
   begin
      Vst3_Log("Called Vst3.Controller.Plain_Param_To_Normalised");
      return 0.0;
   end Plain_Param_To_Normalised;

   function Get_Param_Normalised (This : access Vst3_Controller; Id : Param_Id) return Param_Value is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_Normalised");
      return 0.0;
   end Get_Param_Normalised;

   function Set_Param_Normalised (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_Normalised");
      return Ok_True;
   end Set_Param_Normalised;

   function Set_Component_Handler (This : access Vst3_Controller; Handler : access System.Address) return Result is
   begin 
      Vst3_Log("Called Vst3.Controller.Set_Component_Handler");
      return Ok_True;
   end Set_Component_Handler;

   function Create_View (This : access Vst3_Controller; name : TUID) return access System.Address is
   begin
      Vst3_Log("Called Vst3.Controller.Create_View");
      return null;
   end Create_View;

end Vst3.Controller;
