with System.Atomic_Counters; use System.Atomic_Counters;

package Vst3.Component is 
   Component_Id : constant TUID := Make_TUID (16#E831FF31#, 16#F2D54301#, 16#928EBBEE#, 16#25697802#);
   type Vst3_Component;

   type Component_V_Table is record
      Query_Interface         : access function (This : access Vst3_Component; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                 : access function (This : access Vst3_Component) return Unsigned with Convention => C;
      Release                 : access function (This : access Vst3_Component) return Unsigned with Convention => C;
      Initialise              : access function (This : access Vst3_Component; Context : access F_Unknown) return Result with Convention => C;
      De_Initialise           : access function (This : access Vst3_Component) return Result with Convention => C;
      Get_Controller_Class_Id : access function (This : access Vst3_Component; Class_Id : access TUID) return Result with Convention => C; 
      Set_Io_Mode             : access function (This : access Vst3_Component; Mode : Io_Modes) return Result;
      Get_Bus_Count           : access function (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions) return Int with Convention => C;
      Get_Bus_Info            : access function (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; Info : access Bus_Info) return Result with Convention => C;  
      Get_Routing_Info        : access function (This : access Vst3_Component; Input_Info: access Routing_Info; Output_Info : access Routing_Info) return Result; 
      Activate_Bus            : access function (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; State : C_Bool) return Result; 
      Set_Active              : access function (This : access Vst3_Component; State : C_Bool) return Result;  
      -- TODO(edg): Do we need to implement these properly? These are placeholders right now
      -- Set_State               : access function (This : access Component; arg2 : access Steinberg_IBStream) return Result;  
      Set_State               : access function (This : access Vst3_Component; Stream : access Address) return Result;  
      -- Get_State               : access function (This : access Component; arg2 : access Steinberg_IBStream) return Result; 
      Get_State               : access function (This : access Vst3_Component; Stream : access Address) return Result; 
   end record
   with Convention => C;

   function Query_Interface (This : access Vst3_Component; Interface_Id : TUID; Obj : access Address) 
      return Result
      with Convention => C;

   function Add_Ref (This : access Vst3_Component)
      return Unsigned
      with Convention => C;

   function Release (This : access Vst3_Component) 
      return Unsigned
      with Convention => C;

   function Initialise (This : access Vst3_Component; Context : access F_Unknown) 
      return Result 
      with Convention => C;

   function De_Initialise (This : access Vst3_Component) 
      return Result 
      with Convention => C;

   function Get_Controller_Class_Id (This : access Vst3_Component; Class_Id : access TUID) 
      return Result 
      with Convention => C; 

   function Set_Io_Mode (This : access Vst3_Component; Mode : Io_Modes) 
      return Result 
      with Convention => C; 

   function Get_Bus_Count (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions) 
      return Int
      with Convention => C;

   function Get_Bus_Info (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; Info : access Bus_Info)
      return Result 
      with Convention => C;  

   function Get_Routing_Info (This : access Vst3_Component; Input_Info: access Routing_Info; Output_Info : access Routing_Info)
      return Result 
      with Convention => C; 

   function Activate_Bus (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; State : C_Bool)
      return Result 
      with Convention => C; 

   function Set_Active (This : access Vst3_Component; State : C_Bool)
      return Result
      with Convention => C;

   function Set_State (This : access Vst3_Component; Stream : access Address)
      return Result
      with Convention => C;

   function Get_State (This : access Vst3_Component; Stream : access Address)
      return Result 
      with Convention => C; 

   Table : aliased constant Component_V_Table := (
      Query_Interface         => Query_Interface'Access,
      Add_Ref                 => Add_Ref'Access,
      Release                 => Release'Access,
      Initialise              => Initialise'Access,
      De_Initialise           => De_Initialise'Access,
      Get_Controller_Class_Id => Get_Controller_Class_Id'Access,
      Set_Io_Mode             => Set_Io_Mode'Access,
      Get_Bus_Count           => Get_Bus_Count'Access,
      Get_Bus_Info            => Get_Bus_Info'Access,
      Get_Routing_Info        => Get_Routing_Info'Access,
      Activate_Bus            => Activate_Bus'Access,
      Set_Active              => Set_Active'Access,
      Set_State               => Set_State'Access,
      Get_State               => Get_State'Access
   );

   type Vst3_Component is record
      V_Table : access constant Component_V_Table := Table'Access;
      Ref_Count : aliased Atomic_Unsigned := 0;
   end record;

end Vst3.Component;
