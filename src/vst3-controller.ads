
with System.Atomic_Counters; use System.Atomic_Counters;
with Interfaces.C; use Interfaces.C; 

package Vst3.Controller is 
   type Vst3_Controller;

   Controller_Id : constant TUID := Make_TUID(16#DCD7BBE3#, 16#7742448D#, 16#A874AACC#, 16#979C759E#);

   type Parameter_Flag_Option is (
      NoFlags,
      CanAutomate,
      IsReadOnly,
      IsWrapAround,
      IsList,
      IsHidden,
      IsProgramChange,
      IsBypass
   );

   type Parameter_Flag_Options is array(Parameter_Flag_Option) of Unsigned_32;
   Parameter_Flags : Parameter_Flag_Options := (
      NoFlags           => 0,
      CanAutomate       => 1,
      IsReadOnly        => Shift_Right(1, 1),
      IsWrapAround      => Shift_Right(1, 2),
      IsList            => Shift_Right(1, 3),
      IsHidden          => Shift_Right(1, 4),
      IsProgramChange   => Shift_Right(1, 15),
      IsBypass          => Shift_Right(1, 16)
   );


   type Parameter_Info is record
      Id : aliased Param_Id;
      Title : aliased C_Wide_String_128;
      Short_Title : aliased C_Wide_String_128;
      Units : aliased C_Wide_String_128; 
      Step_Count : aliased Int;  
      Default_Normalised_Value: aliased Param_Value;
      Unit_Id : aliased Int;
      Flags : aliased Int; 
   end record
   with Convention => C_Pass_By_Copy;  

   type Controller_V_Table is record
      Query_Interface            : access function (This : access Vst3_Controller; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                    : access function (This : access Vst3_Controller) return Unsigned with Convention => C;
      Release                    : access function (This : access Vst3_Controller) return Unsigned with Convention => C;
      Initialise                 : access function (This : access Vst3_Controller; Context : access F_Unknown) return Result with Convention => C;
      De_Initialise              : access function (This : access Vst3_Controller) return Result with Convention => C;
      -- TODO(edg): Do we need to implement these properly? These are placeholders right now
      -- Set_Component_State     : access function (arg1 : System.Address; arg2 : access Steinberg_IBStream) return Steinberg_tresult;  -- ./vst3_c_api.h:2514
      Set_Component_State        : access function (This : access Vst3_Controller; Stream : access Address) return Result with Convention => C;  
      -- Set_State               : access function (This : access Controller; arg2 : access Steinberg_IBStream) return Result;  
      Set_State                  : access function (This : access Vst3_Controller; Stream : access Address) return Result with Convention => C;  
      -- Get_State               : access function (This : access Controller; arg2 : access Steinberg_IBStream) return Result; 
      Get_State                  : access function (This : access Vst3_Controller; Stream : access Address) return Result with Convention => C; 
      Get_Parameter_Count        : access function (This : access Vst3_Controller) return Int with Convention => C;  
      Get_Parameter_Info         : access function (This : access Vst3_Controller; Index : Int; Info : access Parameter_Info) return Result with Convention => C;  
      Get_Param_String_By_Value  : access function (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value; String : access C_Wide_String_128) return Result with Convention => C;
      Get_Param_Value_By_String  : access function (This : access Vst3_Controller; Id : Param_Id; String : access C_Wide_String_128; Value : access Param_Value) return Result with Convention => C;
      Normalised_Param_To_Plain  : access function (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Param_Value with Convention => C;
      Plain_Param_To_Normalised  : access function (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Param_Value with Convention => C;
      Get_Param_Normalised       : access function (This : access Vst3_Controller; Id : Param_Id) return Param_Value with Convention => C;
      Set_Param_Normalised       : access function (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Result with Convention => C;
      -- TODO(edg): Do we need to implement IComponentHandler too?
      -- Set_Component_Handler : access function (arg1 : System.Address; arg2 : access Steinberg_Vst_IComponentHandler) return Steinberg_tresult;  -- ./vst3_c_api.h:2525
      Set_Component_Handler      : access function (This : access Vst3_Controller; Handler : access System.Address) return Result with Convention => C;
      -- TODO(edg): Platform specific UI?
      -- Create_View : access function (arg1 : System.Address; arg2 : Steinberg_FIDString) return access Steinberg_IPlugView;  -- ./vst3_c_api.h:2526
      Create_View                : access function (This : access Vst3_Controller; name : TUID) return access System.Address with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

   type Param_Display is digits 4 range Param_Value'First .. Param_Value'Last;

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

   function Get_Parameter_Info (This : access Vst3_Controller; Index : Int; Info : access Parameter_Info)  
      return Result 
      with Convention => C;  

   function Get_Param_String_By_Value (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value; Display : access C_Wide_String_128) 
      return Result
      with Convention => C;

   function Get_Param_Value_By_String (This : access Vst3_Controller; Id : Param_Id; Input : access C_Wide_String_128; Value : access Param_Value)
      return Result 
      with Convention => C;

   function Normalised_Param_To_Plain (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) 
      return Param_Value 
      with Convention => C;

   function Plain_Param_To_Normalised (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) 
      return Param_Value 
      with Convention => C;

   function Get_Param_Normalised (This : access Vst3_Controller; Id : Param_Id) 
      return Param_Value 
      with Convention => C;

   function Set_Param_Normalised (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value)
      return Result 
      with Convention => C;

   function Set_Component_Handler (This : access Vst3_Controller; Handler : access System.Address) 
      return Result 
      with Convention => C;

   function Create_View (This : access Vst3_Controller; name : TUID) 
      return access System.Address
      with Convention => C;

   Table : aliased constant Controller_V_Table := (
      Query_Interface            => Query_Interface'Access,
      Add_Ref                    => Add_Ref'Access,
      Release                    => Release'Access,
      Initialise                 => Initialise'Access,
      De_Initialise              => De_Initialise'Access,
      Set_Component_State        => Set_Component_State'Access,
      Set_State                  => Set_State'Access,
      Get_State                  => Get_State'Access,
      Get_Parameter_Count        => Get_Parameter_Count'Access,
      Get_Parameter_Info         => Get_Parameter_Info'Access,
      Get_Param_String_By_Value  => Get_Param_String_By_Value'Access,
      Get_Param_Value_By_String  => Get_Param_Value_By_String'Access,
      Normalised_Param_To_Plain  => Normalised_Param_To_Plain'Access,
      Plain_Param_To_Normalised  => Plain_Param_To_Normalised'Access,
      Get_Param_Normalised       => Get_Param_Normalised'Access,
      Set_Param_Normalised       => Set_Param_Normalised'Access,
      Set_Component_Handler      => Set_Component_Handler'Access,
      Create_View                => Create_View'Access
   );

   type Vst3_Controller is record
      V_Table     : access constant Controller_V_Table := Table'Access;
      Ref_Count   : aliased Atomic_Unsigned := 0;
      Dummy       : Param_Value;
   end record
   with Convention => C_Pass_By_Copy;
end Vst3.Controller;
