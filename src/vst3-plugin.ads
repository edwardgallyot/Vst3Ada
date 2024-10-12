with System.Atomic_Counters; use System.Atomic_Counters;

package Vst3.Plugin is 
   I_Component_IID : constant TUID := Make_TUID (16#E831FF31#, 16#F2D54301#, 16#928EBBEE#, 16#25697802#);
   type Component;

   type IoModes is (Simple, Advanced, Offline_Processing) with Convention => C; 
   for IoModes use (Simple => 0, Advanced => 1, Offline_Processing => 2 );

   type Media_Types is (Audio, Event) with Convention => C;
   for Media_Types use (Audio => 0, Event => 1);

   type Bus_Directions is (Input, Output) with Convention => C;
   for Bus_Directions use (Input => 0, Output => 1);

   type Bus_Types is (Main, Aux) with Convention => C;
   for Bus_Types use (Main => 0, Aux => 1);

   type Bus_Name is array (1 .. 128) of char16_t;

   type Bus_Info is record
      Media_Type     : aliased Media_Types;  
      Bus_Direction  : aliased Bus_Directions;  
      Channel_Count  : aliased Int;  
      Name           : aliased Bus_Name;
      Bus_Type       : aliased Bus_Types;  
      Flags          : aliased Unsigned_32;
   end record
   with Convention => C_Pass_By_Copy;  -- ./vst3_c_api.h:1691

   type Routing_Info is record
      Media_Type  : aliased Media_Types; 
      Bus_Index   : aliased Int; 
      Channel     : aliased Int;  
   end record
   with Convention => C_Pass_By_Copy;  -- ./vst3_c_api.h:1704


   function Add_Ref (This : access Component)
      return Unsigned
      with Convention => C;

   function Release (This : access Component) 
      return Unsigned
      with Convention => C;

   function Query_Interface (This : access Component; Interface_Id : TUID; Obj : access Address) 
      return Result
      with Convention => C;

   function Initialise (This : access Component; Context : access Unknown) 
      return Result 
      with Convention => C;

   function De_Initialise (This : access Component) 
      return Result 
      with Convention => C;

   function Get_Controller_Class_Id (This : access Component; Class_Id : out TUID) 
      return Result 
      with Convention => C; 

   function Set_Io_Mode (This : access Component; Mode : IoModes) 
      return Result 
      with Convention => C; 

   function Get_Bus_Count (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions) 
      return Int
      with Convention => C;

   function Get_Bus_Info (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; Info : access Bus_Info)
      return Result 
      with Convention => C;  

   function Get_Routing_Info (This : access Component; Input_Info: access Routing_Info; Output_Info : access Routing_Info)
      return Result 
      with Convention => C; 

   function Activate_Bus (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; State : C_Bool)
      return Result 
      with Convention => C; 

   function Set_Active (This : access Component; State : C_Bool)
      return Result
      with Convention => C;

   function Set_State (This : access Component; Stream : access Address)
      return Result
      with Convention => C;

   function Get_State (This : access Component; Stream : access Address)
      return Result 
      with Convention => C; 

   type I_Component_V_Table is record
      Query_Interface         : access function (This : access Component; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                 : access function (This : access Component) return Unsigned with Convention => C;
      Release                 : access function (This : access Component) return Unsigned with Convention => C;
      Initialise              : access function (This : access Component; Context : access Unknown) return Result with Convention => C;
      De_Initialise           : access function (This : access Component) return Result with Convention => C;
      Get_Controller_Class_Id : access function (This : access Component; Class_Id : out TUID) return Result with Convention => C; 
      Set_Io_Mode             : access function (This : access Component; Mode : IoModes) return Result;
      Get_Bus_Count           : access function (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions) return Int with Convention => C;
      Get_Bus_Info            : access function (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; Info : access Bus_Info) return Result with Convention => C;  
      Get_Routing_Info        : access function (This : access Component; Input_Info: access Routing_Info; Output_Info : access Routing_Info) return Result; 
      Activate_Bus            : access function (This : access Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; State : C_Bool) return Result; 
      Set_Active              : access function (This : access Component; State : C_Bool) return Result;  
      Set_State               : access function (This : access Component; Stream : access Address) return Result;  
      Get_State               : access function (This : access Component; Stream : access Address) return Result; 
      -- TODO(edg): Do we need to implement these?
      -- Set_State               : access function (This : access Component; arg2 : access Steinberg_IBStream) return Result;  
      -- Get_State               : access function (This : access Component; arg2 : access Steinberg_IBStream) return Result; 
   end record
   with Convention => C;

   type Component is record
      V_Table : access I_Component_V_Table := new I_Component_V_Table'(
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
      Ref_Count : aliased Atomic_Unsigned := 0;
   end record;
end Vst3.Plugin;