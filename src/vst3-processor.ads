-- TODO(edg): Implement this!
with System.Atomic_Counters; use System.Atomic_Counters;
with Interfaces.C; use Interfaces.C;
with System; use System;

package Vst3.Processor is 
   type Process_Modes is 
     (Realtime,
      Prefetch,
      Offline)
   with Convention => C;
   for Process_Modes use (
      Realtime => 0,
      Prefetch => 1,
      Offline  => 2
   );

   type Sample_Sizes is 
     (Sample_32,
      Sample_64)
   with Convention => C;
   for Sample_Sizes use (
      Sample_32 => 0,
      Sample_64 => 1);

   subtype Speaker_Arrangment is Unsigned_64;

   type Speaker is (
      Unknown,
      Left,
      Right,
      Mono);

   -- NOTE(edg): I only have these channel counts so I'm not going to 
   type Speaker_Flag_Options is array (Speaker) of Unsigned_64;
   Speaker_Flags : Speaker_Flag_Options := (
      Unknown => 0,
      Left  => 1,
      Right => Shift_Left (1, 1),
      Mono  => Shift_Left (1, 19));

   -- TODO(edg): I think we need a process context but I'm not sure!

   type Param_Value_Queue_V_Table is record
      Query_Interface  : access function (This : Address; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref          : access function (This : Address) return Unsigned with Convention => C;
      Release          : access function (This : Address) return Unsigned with Convention => C;
      Get_Parameter_Id : access function (This : Address) return Param_Id;
      Get_Point_Count  : access function (This : Address) return Int;
      Get_Point        : access function (This : Address; arg2 : Int; arg3 : access Int; arg4 : access Param_Value) return Result;
      Add_Point        : access function (This : Address; arg2 : Int; arg3 : Param_Value; arg4 : access Int) return Result;
   end record
   with Convention => C_Pass_By_Copy;

   type Param_Value_Queue is record
      lpVtbl : access Param_Value_Queue_V_Table;
   end record
   with Convention => C_Pass_By_Copy;  

   type Parameter_Changes_V_Table is record
      Query_Interface      : access function (This : Address; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref              : access function (This : Address) return Unsigned with Convention => C;
      Release              : access function (This : Address) return Unsigned with Convention => C;
      Get_Parameter_Count  : access function (This : Address) return Int with Convention => C; 
      Get_Parameter_Data   : access function (This : Address; Index : Int) return access Param_Value_Queue;
      Add_Parameter_Data   : access function (This : Address; Id : access Param_Id; Index : access Int) return access Param_Value_Queue;
   end record
   with Convention => C_Pass_By_Copy;  -- ./vst3_c_api.h:3401

   type Parameter_Changes is record
      lpVtbl : access Parameter_Changes_V_Table;
   end record
   with Convention => C_Pass_By_Copy; 

   type Channel_Buffers ( Sample_Size : Sample_Sizes := Sample_32) is record
      case Sample_Size is
         when Sample_32 =>
            Channel_Buffers_32 : System.Address;
         when Sample_64 =>
            Channel_Buffers_64 : System.Address;
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;

   type Audio_Bus_Buffers is record
      Num_Channels   : aliased Int;
      Silence_Flags  : aliased Unsigned_64;
      Buffers        : aliased Channel_Buffers;
   end record
   with Convention => C_Pass_By_Copy;
   
   type Process_Data is record
      Process_Mode         : aliased Process_Modes;
      Sample_Size          : aliased Sample_Sizes;
      Num_Samples          : aliased Int;
      Num_Inputs           : aliased Int;
      Num_Outputs          : aliased Int;
      Inputs               : access Audio_Bus_Buffers;
      Outputs              : access Audio_Bus_Buffers;  
      -- TODO(edg): Finish this off...
      Input_Param_Changes  : access Parameter_Changes;  -- ./vst3_c_api.h:1940
      Output_Param_Changes : access Parameter_Changes;  -- ./vst3_c_api.h:1941
      -- Input_Events         : access Steinberg_Vst_IEventList;  -- ./vst3_c_api.h:1942
      -- Output_Events        : access Steinberg_Vst_IEventList;  -- ./vst3_c_api.h:1943
      -- Process_Context      : access Steinberg_Vst_ProcessContext;  -- ./vst3_c_api.h:1944
   end record
   with Convention => C_Pass_By_Copy;  -- ./vst3_c_api.h:1931

   Processor_Id : TUID := Make_TUID (16#42043F99#, 16#B7DA453C#, 16#A569E79D#, 16#9AAEC33D#);
   type Vst3_Processor;

   type Process_Setup is record
      Mode              : aliased Process_Modes;
      Sample_Size       : aliased Sample_Sizes; 
      Block_Size        : aliased Int;  
      Sample_Rate       : aliased Long_Float;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Processor_V_Table is record
      Query_Interface         : access function (This : access Vst3_Processor; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref                 : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Release                 : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Set_Bus_Arrangements    : access function (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access  Speaker_Arrangment; Output_Count : Int) return Result with Convention => C;
      Get_Bus_Arrangements    : access function (This : access Vst3_Processor; Bus_Direction : Bus_Directions; Index : Int; Arrangement : access Speaker_Arrangment) return Result with Convention => C;
      Can_Process_Sample_Size : access function (This : access Vst3_Processor; Size : Sample_Sizes) return Result with Convention => C;
      Get_Latency_Samples     : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
      Setup_Processing        : access function (This : access Vst3_Processor; Setup : access Process_Setup) return Result with Convention => C;
      Set_Processing          : access function (This : access Vst3_Processor; State : C_Bool) return Result with Convention => C;
      -- TODO(edg): This needs attention: the Data arg is just a place holder for ProcessData
      --     Process : access function (arg1 : System.Address; arg2 : access Steinberg_Vst_ProcessData) return Steinberg_tresult;  -- ./vst3_c_api.h:3022
      Process                 : access function (This : access Vst3_Processor; Data : access Process_Data) return Result with Convention => C;
      Get_Tail_Samples        : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

   function Get_Speaker_Flags (Channel_Count : Integer) return Unsigned_64;

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

   function Can_Process_Sample_Size (This : access Vst3_Processor; Size : Sample_Sizes)
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

   function Process (This : access Vst3_Processor; Data : access Process_Data)
      return Result 
      with Convention => C;

   function Get_Tail_Samples (This : access Vst3_Processor) 
      return Unsigned 
      with Convention => C;

   Table : aliased constant Processor_V_Table := (
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
      V_Table     : access constant Processor_V_Table := Table'Access;
      Ref_Count   : aliased Atomic_Unsigned           := 0;
      Sample_Rate : Long_Float;
      Block_Size  : Integer;
   end record;
end Vst3.Processor;
