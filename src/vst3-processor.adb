with System.Atomic_Counters; use System.Atomic_Counters;

package body Vst3.Processor is
   function Add_Ref (This : access Vst3_Processor) return Unsigned is 
   begin
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_Processor) return Unsigned is
   begin
      Decrement(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Vst3_Processor;  Interface_Id : TUID; Obj : access Address) return Result is 
   begin
      if Interface_Id = I_Audio_Processor_IID then
         Obj.all := This.all'Address;
         return Ok_True;
      end if;
      return No_Interface;
   end Query_Interface;

   function Set_Bus_Arrangements (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access  Speaker_Arrangment; Output_Count : Int) return Result is
   begin
      return Ok_True;
   end Set_Bus_Arrangements;

   function Get_Bus_Arrangements (This : access Vst3_Processor; Bus_Direction : Bus_Directions; Index : Int; Arrangement : access Speaker_Arrangment) return Result is
   begin
      return Ok_True;
   end Get_Bus_Arrangements;

   function Can_Process_Sample_Size (This : access Vst3_Processor; Size : Int) return Result is
   begin
      return Ok_True;
   end Can_Process_Sample_Size;

   function Get_Latency_Samples (This : access Vst3_Processor) return Unsigned  is
   begin
      return 1;
   end Get_Latency_Samples;

   function Setup_Processing (This : access Vst3_Processor; Setup : access Process_Setup) return Result is
   begin
      return Ok_True;
   end Setup_Processing;

   function Set_Processing (This : access Vst3_Processor; State : C_Bool) return Result is
   begin
      return Ok_True;
   end Set_Processing;

   function Process (This : access Vst3_Processor; Data : access Address) return Result is 
   begin
      return Ok_True;
   end Process;

   function Get_Tail_Samples (This : access Vst3_Processor) return Unsigned is
   begin
      return 1;
   end Get_Tail_Samples;

end Vst3.Processor;
