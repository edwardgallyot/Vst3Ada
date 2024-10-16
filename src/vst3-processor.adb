with System.Atomic_Counters; use System.Atomic_Counters;
with Vst3.Constants; use Vst3.Constants;
with Interfaces.C; use Interfaces.C;

package body Vst3.Processor is
   function Add_Ref (This : access Vst3_Processor) return Unsigned is 
   begin
      Vst3_Log("Called Vst3.Processor.Add_Ref");
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_Processor) return Unsigned is
   begin
      Vst3_Log("Called Vst3.Processor.Release");
      -- TODO(edg): Deallocate. See other release methods.
      Decrement(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Release;

   function Get_Speaker_Flags (Channel_Count : Integer) return Unsigned_64 is
      Res : Unsigned_64 := 0;
   begin
      case Channel_Count is
         when 1 => Res := Speaker_Flags(Mono);
         when 2 => Res := Speaker_Flags(Left) or Speaker_Flags(Right);
         when others => null;
      end case;
      return Res;
   end Get_Speaker_Flags;

   function Query_Interface (This : access Vst3_Processor;  Interface_Id : TUID; Obj : access Address) return Result is 
   begin
      Vst3_Log("Called Vst3.Processor.Query_Interface");
      if Interface_Id = Processor_Id or
         Interface_Id = Unknown_Id
      then
         Obj.all := This.all'Address;
         return Ok_True;
      end if;
      return No_Interface;
   end Query_Interface;

   function Set_Bus_Arrangements (This : access Vst3_Processor; Inputs : access Speaker_Arrangment; Input_Count : Int; Outputs : access  Speaker_Arrangment; Output_Count : Int) return Result is
      -- NOTE(edg): Excellent answer from dcbst here for converting a known C-Size array
      -- to a strong ada type:
      -- https://www.reddit.com/r/ada/comments/1cr493e/array_of_access_type/ A
      Inputs_Array : array (1 .. Input_Count) of Speaker_Arrangment;
      for Inputs_Array'Address use Inputs.all'Address;

      Outputs_Array : array (1 .. Output_Count) of Speaker_Arrangment;
      for Outputs_Array'Address use Outputs.all'Address;
   begin
      Vst3_Log("Called Vst3.Processor.Set_Bus_Arrangements");

      for I in Inputs_Array'Range loop
         Inputs_Array (I) := Get_Speaker_Flags (Channel_Count => 1);
      end loop;

      for I in Outputs_Array'Range loop
         Outputs_Array (I) := Get_Speaker_Flags (Channel_Count => 1);
      end loop;

      return Ok_True;
   end Set_Bus_Arrangements;

   function Get_Bus_Arrangements (This : access Vst3_Processor; Bus_Direction : Bus_Directions; Index : Int; Arrangement : access Speaker_Arrangment) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Get_Bus_Arrangements");
      return Ok_True;
   end Get_Bus_Arrangements;

   function Can_Process_Sample_Size (This : access Vst3_Processor; Size : Int) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Can_Process_Sample_Size");
      return Ok_True;
   end Can_Process_Sample_Size;

   function Get_Latency_Samples (This : access Vst3_Processor) return Unsigned  is
   begin
      Vst3_Log("Called Vst3.Processor.Get_Latency_Samples");
      return 1;
   end Get_Latency_Samples;

   function Setup_Processing (This : access Vst3_Processor; Setup : access Process_Setup) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Setup_Processing");
      return Ok_True;
   end Setup_Processing;

   function Set_Processing (This : access Vst3_Processor; State : C_Bool) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Set_Processing");
      return Ok_True;
   end Set_Processing;

   function Process (This : access Vst3_Processor; Data : access Address) return Result is 
   begin
      -- NOTE(edg): We don't need to spam the audio callback
      return Ok_True;
   end Process;

   function Get_Tail_Samples (This : access Vst3_Processor) return Unsigned is
   begin
      Vst3_Log("Called Vst3.Processor.Get_Tail_Samples");
      return 1;
   end Get_Tail_Samples;

end Vst3.Processor;
