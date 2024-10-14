
with System.Atomic_Counters; use System.Atomic_Counters;
with Interfaces.C; use Interfaces.C; 

package Vst3.Controller is 
   type Vst3_Controller;

   I_Controller_IID : constant TUID := Make_TUID(16#DCD7BBE3#, 16#7742448D#, 16#A874AACC#, 16#979C759E#);


   subtype Steinberg_Vst_ParamID is Unsigned; 
   subtype Steinberg_Vst_ParamValue is Standard.Long_Float;
   subtype Steinberg_Vst_UnitID is Integer;

   type Steinberg_Vst_ParameterInfo is record
      Id : aliased Steinberg_Vst_ParamID;  -- ./vst3_c_api.h:1716
      Title : aliased C_String_128;  -- ./vst3_c_api.h:1717
      Short_Title : aliased C_String_128;  -- ./vst3_c_api.h:1718
      Units : aliased C_String_128; 
      Step_Count : aliased Int;  
      Default_Normalised_Value: aliased Steinberg_Vst_ParamValue;
      Unit_Id : aliased Steinberg_Vst_UnitID;
      Flags : aliased Int; 
   end record
   with Convention => C_Pass_By_Copy;  

   function Query_Interface (This : access Vst3_Controller; Interface_Id : TUID; Obj : access Address) 
      return Result
      with Convention => C;

   function Add_Ref (This : access Vst3_Controller)
      return Unsigned
      with Convention => C;

   function Release (This : access Vst3_Controller) 
      return Unsigned
      with Convention => C;

   function Initialise (This : access Vst3_Controller; Context : access F_Unknown) 
      return Result 
      with Convention => C;

   function De_Initialise (This : access Vst3_Controller) 
      return Result 
      with Convention => C;

   function Set_Component_State (This : access Vst3_Controller; Stream : access Address) 
      return Result
      with Convention => C;  

   function Set_State (This : access Vst3_Controller; Stream : access Address) 
      return Result
      with Convention => C;  

   function Get_State (This : access Vst3_Controller; Stream : access Address) 
      return Result
      with Convention => C;  

   function Get_Parameter_Count (This : access Vst3_Controller)
      return Int 
      with Convention => C;  

   function Get_Parameter_Info (This : access Vst3_Controller; Index : Int; Info : access Steinberg_Vst_ParameterInfo)  
      return Result 
      with Convention => C;  

   type I_Edit_Controller_V_Table is record
      Query_Interface         : access function (This : access Vst3_Controller; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                 : access function (This : access Vst3_Controller) return Unsigned with Convention => C;
      Release                 : access function (This : access Vst3_Controller) return Unsigned with Convention => C;
      Initialise              : access function (This : access Vst3_Controller; Context : access F_Unknown) return Result with Convention => C;
      De_Initialise           : access function (This : access Vst3_Controller) return Result with Convention => C;
      -- TODO(edg): Do we need to implement these properly? These are placeholders right now
      -- Set_Component_State     : access function (arg1 : System.Address; arg2 : access Steinberg_IBStream) return Steinberg_tresult;  -- ./vst3_c_api.h:2514
      Set_Component_State     : access function (This : access Vst3_Controller; Stream : access Address) return Result with Convention => C;  
      -- Set_State               : access function (This : access Controller; arg2 : access Steinberg_IBStream) return Result;  
      Set_State               : access function (This : access Vst3_Controller; Stream : access Address) return Result with Convention => C;  
      -- Get_State               : access function (This : access Controller; arg2 : access Steinberg_IBStream) return Result; 
      Get_State               : access function (This : access Vst3_Controller; Stream : access Address) return Result with Convention => C; 
      Get_Parameter_Count     : access function (This : access Vst3_Controller) return Int with Convention => C;  
      Get_Parameter_Info      : access function (This : access Vst3_Controller; Index : Int; Info : access Steinberg_Vst_ParameterInfo) return Result with Convention => C;  
-- getParamStringByValue : access function
--      (arg1 : System.Address;
--       arg2 : Steinberg_Vst_ParamID;
--       arg3 : Steinberg_Vst_ParamValue;
--       arg4 : access Steinberg_Vst_TChar) return Steinberg_tresult;  -- ./vst3_c_api.h:2519
-- getParamValueByString : access function
--      (arg1 : System.Address;
--       arg2 : Steinberg_Vst_ParamID;
--       arg3 : access Steinberg_Vst_TChar;
--       arg4 : access Steinberg_Vst_ParamValue) return Steinberg_tresult;  -- ./vst3_c_api.h:2520
-- normalizedParamToPlain : access function
--      (arg1 : System.Address;
--       arg2 : Steinberg_Vst_ParamID;
--       arg3 : Steinberg_Vst_ParamValue) return Steinberg_Vst_ParamValue;  -- ./vst3_c_api.h:2521
-- plainParamToNormalized : access function
--      (arg1 : System.Address;
--       arg2 : Steinberg_Vst_ParamID;
--       arg3 : Steinberg_Vst_ParamValue) return Steinberg_Vst_ParamValue;  -- ./vst3_c_api.h:2522
-- getParamNormalized : access function (arg1 : System.Address; arg2 : Steinberg_Vst_ParamID) return Steinberg_Vst_ParamValue;  -- ./vst3_c_api.h:2523
-- setParamNormalized : access function
--      (arg1 : System.Address;
--       arg2 : Steinberg_Vst_ParamID;
--       arg3 : Steinberg_Vst_ParamValue) return Steinberg_tresult;  -- ./vst3_c_api.h:2524
-- setComponentHandler : access function (arg1 : System.Address; arg2 : access Steinberg_Vst_IComponentHandler) return Steinberg_tresult;  -- ./vst3_c_api.h:2525
-- createView : access function (arg1 : System.Address; arg2 : Steinberg_FIDString) return access Steinberg_IPlugView;  -- ./vst3_c_api.h:2526
   end record
   with Convention => C_Pass_By_Copy;

   Table : aliased constant I_Edit_Controller_V_Table := (
      Query_Interface      => Query_Interface'Access,
      Add_Ref              => Add_Ref'Access,
      Release              => Release'Access,
      Initialise           => Initialise'Access,
      De_Initialise        => De_Initialise'Access,
      Set_Component_State  => Set_Component_State'Access,
      Set_State            => Set_State'Access,
      Get_State            => Get_State'Access,
      Get_Parameter_Count   => Get_Parameter_Count'Access,
      Get_Parameter_Info   => Get_Parameter_Info'Access
   );

   type Vst3_Controller is record
      V_Table : access constant I_Edit_Controller_V_Table := Table'Access;
      Ref_Count : aliased Atomic_Unsigned := 0;
   end record
   with Convention => C_Pass_By_Copy;
end Vst3.Controller;
