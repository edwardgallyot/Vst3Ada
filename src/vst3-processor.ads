-- TODO(edg): Implement this!
with System.Atomic_Counters; use System.Atomic_Counters;
with Interfaces.C; use Interfaces.C;

package Vst3.Processor is 
   I_Audio_Processor_IID : TUID := Make_TUID (16#42043F99#, 16#B7DA453C#, 16#A569E79D#, 16#9AAEC33D#);
   type Vst3_Processor;

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

   function Set_Bus_Arrangements (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access  Speaker_Arrangment; Output_Count : Int)
      return Result
      with Convention => C;

   -- function Get_Bus_Arrangements (This : access Vst3_Processor)
   --    return Result
   --    with Convention => C;

  -- -- methods derived from "Steinberg_FUnknown":  
  --     getBusArrangement : access function
  --          (arg1 : System.Address;
  --           arg2 : Steinberg_Vst_BusDirection;
  --           arg3 : Steinberg_int32;
  --           arg4 : access Steinberg_Vst_SpeakerArrangement) return Steinberg_tresult;  -- ./vst3_c_api.h:3017
  --     canProcessSampleSize : access function (arg1 : System.Address; arg2 : Steinberg_int32) return Steinberg_tresult;  -- ./vst3_c_api.h:3018
  --     getLatencySamples : access function (arg1 : System.Address) return Steinberg_uint32;  -- ./vst3_c_api.h:3019
  --     setupProcessing : access function (arg1 : System.Address; arg2 : access Steinberg_Vst_ProcessSetup) return Steinberg_tresult;  -- ./vst3_c_api.h:3020
  --     setProcessing : access function (arg1 : System.Address; arg2 : Steinberg_TBool) return Steinberg_tresult;  -- ./vst3_c_api.h:3021
  --     process : access function (arg1 : System.Address; arg2 : access Steinberg_Vst_ProcessData) return Steinberg_tresult;  -- ./vst3_c_api.h:3022
  --     getTailSamples : access function (arg1 : System.Address) return Steinberg_uint32;  -- ./vst3_c_api.h:3023
  --  end record
  --  with Convention => C_Pass_By_Copy;  -- ./vst3_c_api.h:3008
   type I_Audio_Processor_V_Table is record
      Query_Interface         : access function (This : access Vst3_Processor; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                 : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Release                 : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Set_Bus_Arrangements    : access function (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access  Speaker_Arrangment; Output_Count : Int) return Result with Convention => C;
   end record;

   Table : aliased constant I_Audio_Processor_V_Table := (
      Query_Interface      => Query_Interface'Access,
      Add_Ref              => Add_Ref'Access,
      Release              => Release'Access,
      Set_Bus_Arrangements => Set_Bus_Arrangements'Access
   );

   type Vst3_Processor is record 
      V_Table: access constant I_Audio_Processor_V_Table := Table'Access;
      Ref_Count : aliased Atomic_Unsigned := 0;
   end record;
end Vst3.Processor;
