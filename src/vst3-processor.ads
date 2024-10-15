-- TODO(edg): Implement this!
with System.Atomic_Counters; use System.Atomic_Counters;
with Interfaces.C; use Interfaces.C;
with System; use System;

package Vst3.Processor is 
   I_Audio_Processor_IID : TUID := Make_TUID (16#42043F99#, 16#B7DA453C#, 16#A569E79D#, 16#9AAEC33D#);
   type Vst3_Processor;

   type Process_Setup is record
      Process_Mode      : aliased Int;
      Sample_Size       : aliased Int; 
      Samples_Per_Block : aliased Int;  
      Sample_Rate       : aliased Long_Float;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Speaker_Arrangment is new Unsigned_Long;

   function Query_Interface (This : access Vst3_Processor; Interface_Id : TUID; Obj : access Address) 
      return Result
      with Convention => C;

   function Add_Ref (This : access Vst3_Processor)
      return Unsigned
      with Convention => C;

   function Release (This : access Vst3_Processor) 
      return Unsigned
      with Convention => C;

   function Set_Bus_Arrangements (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access Speaker_Arrangment; Output_Count : Int)
      return Result
      with Convention => C;

   function Get_Bus_Arrangements (This : access Vst3_Processor; Bus_Direction : Bus_Directions; Index : Int; Arrangement : access Speaker_Arrangment)
      return Result
      with Convention => C;

   function Can_Process_Sample_Size (This : access Vst3_Processor; Size : Int)
      return Result 
      with Convention => C;

   function Get_Latency_Samples (This : access Vst3_Processor) 
      return Unsigned 
      with Convention => C;

   function Setup_Processing (This : access Vst3_Processor; Setup : access Process_Setup)
      return Result
      with Convention => C;

   function Set_Processing (This : access Vst3_Processor; State : C_Bool)
      return Result
      with Convention => C;

   function Process (This : access Vst3_Processor; Data : access Address)
      return Result 
      with Convention => C;

   function Get_Tail_Samples (This : access Vst3_Processor) 
      return Unsigned 
      with Convention => C;

   type I_Audio_Processor_V_Table is record
      Query_Interface         : access function (This : access Vst3_Processor; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                 : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Release                 : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Set_Bus_Arrangements    : access function (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access  Speaker_Arrangment; Output_Count : Int) return Result with Convention => C;
      Get_Bus_Arrangements    : access function (This : access Vst3_Processor; Bus_Direction : Bus_Directions; Index : Int; Arrangement : access Speaker_Arrangment) return Result with Convention => C;
      Can_Process_Sample_Size : access function (This : access Vst3_Processor; Size : Int) return Result with Convention => C;
      Get_Latency_Samples     : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Setup_Processing        : access function (This : access Vst3_Processor; Setup : access Process_Setup) return Result with Convention => C;
      Set_Processing          : access function (This : access Vst3_Processor; State : C_Bool) return Result with Convention => C;
      -- TODO(edg): This needs attention: the Data arg is just a place holder for ProcessData
      --     Process : access function (arg1 : System.Address; arg2 : access Steinberg_Vst_ProcessData) return Steinberg_tresult;  -- ./vst3_c_api.h:3022
      Process                 : access function (This : access Vst3_Processor; Data : access Address) return Result with Convention => C;
      Get_Tail_Samples        : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

   Table : aliased constant I_Audio_Processor_V_Table := (
      Query_Interface         => Query_Interface'Access,
      Add_Ref                 => Add_Ref'Access,
      Release                 => Release'Access,
      Set_Bus_Arrangements    => Set_Bus_Arrangements'Access,
      Get_Bus_Arrangements    => Get_Bus_Arrangements'Access,
      Can_Process_Sample_Size => Can_Process_Sample_Size'Access,
      Get_Latency_Samples     => Get_Latency_Samples'Access,
      Setup_Processing        => Setup_Processing'Access,
      Set_Processing          => Set_Processing'Access,
      Process                 => Process'Access,
      Get_Tail_Samples        => Get_Tail_Samples'Access
   );

   type Vst3_Processor is record 
      V_Table: access constant I_Audio_Processor_V_Table := Table'Access;
      Ref_Count : aliased Atomic_Unsigned := 0;
   end record;
end Vst3.Processor;
