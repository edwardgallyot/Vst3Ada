with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;
with System.Atomic_Counters; use System.Atomic_Counters;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Vst3.Config; use Vst3.Config;
with Vst3.Constants; use Vst3.Constants;
with Vst3.Component; use Vst3.Component;
with Vst3.Controller; use Vst3.Controller;
with Vst3.Plugin; use Vst3.Plugin;
with Interfaces.C; use Interfaces.C;
with Vst3.Processor;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Vst3.Processor is
   G : Generator;

   function To_Plugin (Input : access Vst3_Processor) return access Vst3_Plugin is 
      type Vst3_Plugin_Ref is access Vst3_Plugin;
      function Plugin_From_Address is new Ada.Unchecked_Conversion(Address, Vst3_Plugin_Ref);
      Plugin : Vst3_Plugin_Ref := 
         Plugin_From_Address (
            To_Address (
               To_Integer (Input.all'Address) - Vst3_Controller'Size - Vst3_Component'Size));
   begin
      return Plugin; 
   end To_Plugin;

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
      -- https://www.reddit.com/r/ada/comments/1cr493e/array_of_access_type/
      Inputs_Array : array (1 .. Input_Count) of Speaker_Arrangment;
      for Inputs_Array'Address use Inputs.all'Address;

      Outputs_Array : array (1 .. Output_Count) of Speaker_Arrangment;
      for Outputs_Array'Address use Outputs.all'Address;

      Input_Ok : Boolean := True;
      Output_Ok : Boolean := True;
      Requested_Speaker : Speaker_Arrangment := Speaker_Flags(Unknown);
      Accepted_Speaker : Speaker_Arrangment;

      -- TODO(ed): We want to be able to get this from the plugin
      Channel_Count : constant Integer := Vst3_Channel_Count;
   begin
      Vst3_Log("Called Vst3.Processor.Set_Bus_Arrangements");

      for I in Inputs_Array'Range loop
         Requested_Speaker := Inputs_Array(I);
         Accepted_Speaker := Get_Speaker_Flags (Channel_Count);
         Input_Ok := Input_Ok and (Requested_Speaker = Accepted_Speaker or Requested_Speaker = 0);
      end loop;

      for I in Outputs_Array'Range loop
         Requested_Speaker := Outputs_Array(I);
         Accepted_Speaker := Get_Speaker_Flags (Channel_Count);
         Output_Ok := Output_Ok and (Requested_Speaker = Accepted_Speaker or Requested_Speaker = 0);
      end loop;

      return (if Output_Ok and Input_Ok then Ok_True else False);
   end Set_Bus_Arrangements;

   function Get_Bus_Arrangements (This : access Vst3_Processor; Bus_Direction : Bus_Directions; Index : Int; Arrangement : access Speaker_Arrangment) return Result is
      Channel_Count : Integer := Vst3_Channel_Count;
   begin
      Vst3_Log("Called Vst3.Processor.Get_Bus_Arrangements");
      -- Zero based indexing for a mono bus...
      if Index > 0 then
         return False;
      end if;

      case Bus_Direction is 
         when Output => Arrangement.all := Get_Speaker_Flags (Channel_Count);
         when Input => Arrangement.all := Get_Speaker_Flags (Channel_Count);
      end case;

      return Ok_True;
   end Get_Bus_Arrangements;

   function Can_Process_Sample_Size (This : access Vst3_Processor; Size : Sample_Sizes) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Can_Process_Sample_Size");
      return (if Size = Sample_32 then Ok_True else Invalid_Argument);
   end Can_Process_Sample_Size;

   function Get_Latency_Samples (This : access Vst3_Processor) return Unsigned  is
   begin
      Vst3_Log("Called Vst3.Processor.Get_Latency_Samples");
      -- TODO(edg): Get the actual latency in samples that we need.
      return 1;
   end Get_Latency_Samples;

   function Setup_Processing (This : access Vst3_Processor; Setup : access Process_Setup) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Setup_Processing");
      -- TODO(edg): We should use the sample rate and the block size here. 
      This.Sample_Rate := Float(Setup.Sample_Rate);
      This.Block_Size  := Integer(Setup.Block_Size);
      Reset(G);
      return Ok_True;
   end Setup_Processing;

   function Set_Processing (This : access Vst3_Processor; State : C_Bool) return Result is
   begin
      Vst3_Log("Called Vst3.Processor.Set_Processing");
      -- NOTE(edg): Maybe we don't care here?
      return Ok_True;
   end Set_Processing;

   function Process (This : access Vst3_Processor; Data : access Process_Data) return Result is 
      Bus_Inputs : array (1 .. Data.Num_Inputs) of Audio_Bus_Buffers with Import;
      for Bus_Inputs'Address use Data.Inputs.all'Address;

      Bus_Outputs : array (1 .. Data.Num_Outputs) of Audio_Bus_Buffers with Import;
      for Bus_Outputs'Address use Data.Outputs.all'Address;
   begin
      -- NOTE(edg): We don't need to spam the audio callback
      for Bus_Input of Bus_Inputs loop
         declare 
            Input_Channels : array (1 .. Bus_Input.Num_Channels, 1 .. Data.Num_Samples) of Float;
            for Input_Channels'Address use Bus_Input.Buffers.Channel_Buffers_32.all.all'Address;
         begin
            for Channel in Input_Channels'Range(1) loop
               for Sample in Input_Channels'Range(2) loop 
                  Input_Channels(Channel, Sample) := 0.0;
                  null;
               end loop;
            end loop;
         end;
      end loop;

      for Bus_Output of Bus_Outputs loop
         declare 
            Output_Channels : array (1 .. Bus_Output.Num_Channels, 1 .. Data.Num_Samples) of Float;
            for Output_Channels'Address use Bus_Output.Buffers.Channel_Buffers_32.all.all'Address;

            Sin_Delta : constant Float := (400.0 / This.Sample_Rate) * 2.0 * Pi;
         begin

            for Sample in Output_Channels'Range(2) loop 
               This.Sin_Phase := (This.Sin_Phase + Sin_Delta);
               declare
                  Out_Sample : Float := Sin (This.Sin_Phase);
               begin

                  if This.Sin_Phase >= 2.0 * Pi then 
                     This.Sin_Phase := This.Sin_Phase - (2.0 * Pi);
                  end if;

                  for Channel in Output_Channels'Range(1) loop
                     Output_Channels(Channel, Sample) := Out_Sample;
                  end loop;

               end;
            end loop;
         end;
      end loop;
      return Ok_True;
   end Process;

   function Get_Tail_Samples (This : access Vst3_Processor) return Unsigned is
   begin
      Vst3_Log("Called Vst3.Processor.Get_Tail_Samples");
      return 1;
   end Get_Tail_Samples;

end Vst3.Processor;
