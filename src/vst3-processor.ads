with System.Atomic_Counters; use System.Atomic_Counters;
with Interfaces.C; use Interfaces.C;
with System; use System;
with Ada.Numerics; use Ada.Numerics;

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

   -- NOTE(edg): I only have these channel counts as that's all I need...
   -- TODO(edg): Implment all the constants?
   type Speaker_Flag_Options is array (Speaker) of Unsigned_64;
   Speaker_Flags : Speaker_Flag_Options := (
      Unknown => 0,
      Left  => 1,
      Right => Shift_Left (1, 1),
      Mono  => Shift_Left (1, 19));
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
      V_Table : access Param_Value_Queue_V_Table;
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
   with Convention => C_Pass_By_Copy; 

   type Parameter_Changes is record
      V_Table : access Parameter_Changes_V_Table;
   end record
   with Convention => C_Pass_By_Copy; 

   type Note_On_Event is record
      Channel  : aliased Short;  
      Pitch    : aliased Short;  
      Tuning   : aliased Float;  
      Velocity : aliased Float;  
      Length   : aliased Int;  
      Note_Id  : aliased Int;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Note_Off_Event is record
      Channel  : aliased Short;  
      Pitch    : aliased Short;  
      Velocity : aliased Float;  
      Note_Id  : aliased Int;  
      Tuning   : aliased Float;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Data_Event is record
      Size        : aliased Unsigned;  
      Data_Type   : aliased Unsigned;  
      Bytes       : access Unsigned_Char;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Poly_Pressure_Event is record
      Channel  : aliased Short;  
      Pitch    : aliased Short;  
      Pressure : aliased Float;  
      Note_Id  : aliased Int;  
   end record
   with Convention => C_Pass_By_Copy;  

   subtype Note_Expression_Type_Id is Unsigned;
   subtype Note_Expression_Value is Long_Float;

   type Note_Expression_Value_Event is record
      Type_Id  : aliased Note_Expression_Type_Id;  
      Note_Id  : aliased Int;  
      Value    : aliased Note_Expression_Value;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Note_Expression_Text_Event is record
      Type_Id  : aliased Note_Expression_Type_Id;  
      Note_Id  : aliased Int;  
      Text_Len : aliased Unsigned;  
      Text     : access Char16_T;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Chord_Event is record
      Root        : aliased Short;  
      Bass_Note   : aliased Short;  
      Mask        : aliased Short;  
      Text_Len    : aliased Unsigned_Short;  
      Text        : access Char16_T;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Scale_Event is record
      root     : aliased Short;  
      mask     : aliased Short;  
      textLen  : aliased Unsigned_Short;  
      text     : access Char16_T;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Legacy_Midi_Cc_Out_Event is record
      Control_Number : aliased Unsigned_Char;  
      channel        : aliased Char;  
      Value          : aliased Char;  
      Value_2        : aliased Char;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Event_Data (Event_Type : unsigned := 0) is record
      case Event_Type is
         when 0 =>
            Note_On                 : aliased Note_On_Event;  
         when 1 =>
            Note_Off                : aliased Note_Off_Event;  
         when 2 =>
            Data                    : aliased Data_Event;  
         when 3 =>
            Poly_Pressure           : aliased Poly_Pressure_Event;  
         when 4 =>
            Note_Expression_Value   : aliased Note_Expression_Value_Event;  
         when 5 =>
            Note_Expression_Text    : aliased Note_Expression_Text_Event;  
         when 6 =>
            Chord                   : aliased Chord_Event;  
         when 7 =>
            Scale                   : aliased Scale_Event;  
         when others =>
            Midi_Cc_Out             : aliased Legacy_Midi_Cc_Out_Event;  
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;

   subtype Quarter_Notes is Long_Float;

   type Event is record
      Bus_Index      : aliased Int;  
      Sample_Offset  : aliased Int;  
      Ppq_Position   : aliased Quarter_Notes;  
      Flags          : aliased Unsigned_Short;  
      Event_Type     : aliased Unsigned_Short;  
      Data           : aliased Event_Data;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Event_List_V_Table is record
      Query_Interface  : access function (This : Address; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref          : access function (This : Address) return Unsigned with Convention => C;
      Release          : access function (This : Address) return Unsigned with Convention => C;
      Get_Event_Count  : access function (This : System.Address) return Int;
      Get_Event        : access function (This : System.Address; Index : Int; Event_List : access Event) return Result; 
      Add_Event        : access function (This : System.Address; Event_List : access Event) return Result;
   end record
   with Convention => C_Pass_By_Copy;  

   type Event_List is record
      V_Table : access Event_List_V_Table;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Channel_Data_32 is access Float;
   type Channel_Access_32 is access Channel_Data_32;
   
   type Channel_Data_64 is access Long_Float;
   type Channel_Access_64 is access Channel_Data_64;

   type Channel_Buffers ( Sample_Size : Sample_Sizes := Sample_32) is record
      case Sample_Size is
         when Sample_32 =>
            Channel_Buffers_32 : Channel_Access_32;
         when Sample_64 =>
            Channel_Buffers_64 : Channel_Access_64;
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;

   type Audio_Bus_Buffers is record
      Num_Channels   : aliased Integer_32;
      Silence_Flags  : aliased Unsigned_64;
      Buffers        : aliased Channel_Buffers;
   end record
   with Convention => C_Pass_By_Copy;

   type Vst_Chord is record
      Key_Note    : Unsigned_8;
      Root_Note   : Unsigned_8;
      Chord_Mask  : Integer_16;
   end record
   with Convention => C_Pass_By_Copy;

   type Vst_Frame_Rate is record
      Frames_Per_Second : Unsigned_32;
      Flags             : Unsigned_32;
   end record
   with Convention => C_Pass_By_Copy;

   subtype Time_Samples is Unsigned_64;

   type Process_Context is record
      State                      : aliased Unsigned_32;  
      Sample_Rate                : aliased Long_Float;  
      Project_Time_Samples       : aliased Time_Samples;  
      System_Time                : aliased Integer;  
      Continous_Time_Samples     : aliased Time_Samples;  
      Project_Time_Music         : aliased Quarter_Notes;  
      Bar_Position_Music         : aliased Quarter_Notes;  
      Cycle_Start_Music          : aliased Quarter_Notes;  
      Cycle_End_Music            : aliased Quarter_Notes;  
      Tempo                      : aliased Double;  
      Time_Signature_Numerator   : aliased Integer_32;  
      Time_Signature_Denominator : aliased Integer_32;  
      Chord                      : aliased Vst_Chord;  
      Smpte_Offset_Subframes     : aliased Integer_32;  
      Frame_Rate                 : aliased Vst_Frame_Rate;  
      Samples_To_Next_Clock      : aliased Integer_32;  
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
      Input_Param_Changes  : access Parameter_Changes;  
      Output_Param_Changes : access Parameter_Changes;  
      Input_Events         : access Event_List;  
      Output_Events        : access Event_List;  
      Context              : access Process_Context;  
   end record
   with Convention => C_Pass_By_Copy;  

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
      Process                 : access function (This : access Vst3_Processor; Data : access Process_Data) return Result with Convention => C;
      Get_Tail_Samples        : access function (This : access Vst3_Processor) return Unsigned with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

   type Parameter_Type is (
      Bypass,
      Gain);

   for Parameter_Type use (
      Bypass => 0,
      Gain => 1);

   type Parameter_List is array (Parameter_Type) of Parameter_Info;
   Parameter_Infos : Parameter_List := (

      Bypass => (
         Id => 0,
         Title => To_C ("Bypass"),
         Short_Title => To_C("Byps"),
         Units => To_C (""),
         Step_Count => 1,
         Default_Normalised_Value => 1.0,
         Unit_Id => 0,
         Flags => Int(Parameter_Flags(IsBypass) or Parameter_Flags(CanAutomate))
      ),

      Gain => (
         Id => 1,
         Title => To_C ("Gain"),
         Short_Title => To_C("Gain"),
         Units => To_C("dB"),
         Step_Count => 1024,
         Default_Normalised_Value => 1.0,
         Unit_Id => 0,
         Flags => Int(Parameter_Flags(CanAutomate))
      ));

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
      Sample_Rate : Float;
      Block_Size  : Integer;
      Sin_Phase   : Float;
   end record;

end Vst3.Processor;
