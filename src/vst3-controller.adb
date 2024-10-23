with System.Atomic_Counters; use System.Atomic_Counters;
with Vst3;
with Vst3.Constants; use Vst3.Constants;
with Vst3.Config; use Vst3.Config;
with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;
with Ada.Long_Float_Wide_Text_IO; use Ada.Long_Float_Wide_Text_IO;
with Vst3.Controller;
with Vst3.Processor; use Vst3.Processor;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions; use Ada.Numerics.Long_Elementary_Functions;

package body Vst3.Controller is 
   function Add_Ref (This : access Vst3_Controller) return Unsigned is 
   begin
      Vst3_Log("Called Vst3.Controller.Add_Ref");
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_Controller) return Unsigned is
   begin
      Vst3_Log("Called Vst3.Controller.Release");
      Decrement(This.Ref_Count);
      -- TODO(edg): We need to deallocate the memory here but not for now.
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Vst3_Controller; Interface_Id: access TUID; Obj : access Address) return Result is 
      Unused : Unsigned;
   begin
      Vst3_Log("Called Vst3.Controller.Query_Interface with Id: " & Interface_Id.all'Image);
      if Interface_Id.all = Controller_Id  or 
         Interface_Id.all = Unknown_Id     or
         Interface_Id.all = Plugin_Base_Id 
      then
         Vst3_Log("Accepted Interface with Id: " & Interface_Id.all'Image);
         Obj.all := This.all'Address;
         Unused := Add_Ref (This);

         -- TODO(edg): We should have a look at MIDI mapping here
         return Ok_True;
      end if;
      return No_Interface;
   end Query_Interface;

   function Initialise (This : access Vst3_Controller; Context : access F_Unknown) return Result is 
   begin
      Vst3_Log("Called Vst3.Controller.Initialise");
      return Ok_True;
   end Initialise;

   function De_Initialise (This : access Vst3_Controller) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.De_Initialise");
      return Ok_True;
   end De_Initialise;

   function Set_Component_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Set_Component_State");
      return Not_Implemented;
   end Set_Component_State;

   function Set_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Set_State");
      return Not_Implemented;
   end Set_State;

   function Get_State (This : access Vst3_Controller; Stream : access Address) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_State");
      return Not_Implemented;
   end Get_State;

   function Get_Parameter_Count (This : access Vst3_Controller) return Int is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Parameter_Count");
      return Int(Vst3_Num_Parameters);
   end Get_Parameter_Count;

   function Get_Parameter_Info (This : access Vst3_Controller; Index : Int; Info : access Parameter_Info) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Parameter_Info for Index: " & Index'Image);

      if Index < Parameter_Type'Enum_Rep(Parameter_Type'First) or 
         Index > Parameter_Type'Enum_Rep(Parameter_Type'Last) then
         return False;
      end if;
      Info.all := Parameter_Infos (Parameter_Type'Val(Index));
      return Ok_True;
   end Get_Parameter_Info;

   function Get_Param_String_By_Value (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value; Display : access C_Wide_String_128) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_String_By_Value Value: " & Value'Image & ", Id: " & Id'Image);
      declare
         Parameter : Parameter_Type := Parameter_Type'Enum_Val(Id);
      begin
         case Parameter is 
            when Bypass => Display.all := (if Value > 0.5 then To_C ("On") else To_C ("Off"));
            when others => 
               declare 
                  Res : Wide_String (1 .. 64);
                  Value_To_Print : Param_Value := Normalised_Param_To_Plain(This, Id, Value);
               begin
                  -- Copied from: https://stackoverflow.com/questions/64238121/scientific-notation-and-non-scientific-notation-values-in-one-line
                  if Value_To_Print /= Long_Float'First then
                     Put (To  => Res,
                        Item  => Value_To_Print,
                        Aft   => 3,
                        Exp   => 0);
                     Display.all := To_C ((Trim (Res, Ada.Strings.Both)));
                  else
                     Display.all := To_C ("-inf");
                  end if;
               end;
         end case;
      end;
      return Ok_True;
   end Get_Param_String_By_Value;

   function Get_Param_Value_By_String (This : access Vst3_Controller; Id : Param_Id; Input : access C_Wide_String_128; Value : access Param_Value) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_Value_By_String");
      if Id < Parameter_Type'Enum_Rep(Parameter_Type'First) or 
         Id > Parameter_Type'Enum_Rep(Parameter_Type'Last) then
         return Invalid_Argument;
      end if;

      declare
         Parameter : Parameter_Type := Parameter_Type'Enum_Val(Id);
      begin
         -- TODO(edg): Fill this in. If we need to...
         case Parameter is 
            when Gain => null;
            when Bypass => null;
         end case;
      end;
      return Not_Implemented;
   end Get_Param_Value_By_String;

   function Normalised_Param_To_Plain (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Param_Value  is
   begin
      Vst3_Log("Called Vst3.Controller.Normalised_Param_To_Plain");

      if Id < Parameter_Type'Enum_Rep(Parameter_Type'First) or 
         Id > Parameter_Type'Enum_Rep(Parameter_Type'Last) then
         return 0.0;
      end if;

      declare
         Parameter : Parameter_Type := Parameter_Type'Enum_Val(Id);
      begin
         case Parameter is 
            when Gain => return Param_Value((if Value /= 0.0 then 20.0 * Log(Value, 10.0) else Long_Float'First));
            when Bypass => return Value;
         end case;
      end;
   end Normalised_Param_To_Plain;

   function Plain_Param_To_Normalised (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Param_Value  is
   begin
      Vst3_Log("Called Vst3.Controller.Plain_Param_To_Normalised");
      if Id < Parameter_Type'Enum_Rep(Parameter_Type'First) or 
         Id > Parameter_Type'Enum_Rep(Parameter_Type'Last) then
         return 0.0;
      end if;

      declare
         Parameter : Parameter_Type := Parameter_Type'Enum_Val(Id);
      begin
         case Parameter is 
            when Bypass => return Value;
            when Gain => return Param_Value(10.0 ** (Value / 20.0));
         end case;
      end;
   end Plain_Param_To_Normalised;

   function Get_Param_Normalised (This : access Vst3_Controller; Id : Param_Id) return Param_Value is
   begin
      Vst3_Log("Called Vst3.Controller.Get_Param_Normalised, Id: " & Id'Image);

      if Id < Parameter_Type'Enum_Rep(Parameter_Type'First) or 
         Id > Parameter_Type'Enum_Rep(Parameter_Type'Last) then
         return 0.0;
      end if;

      declare
         Parameter : Parameter_Type := Parameter_Type'Enum_Val(Id);
      begin
         return Plain_Param_To_Normalised(This, Id, This.Parameter_Cache(Parameter));
      end;
   end Get_Param_Normalised;

   function Set_Param_Normalised (This : access Vst3_Controller; Id : Param_Id; Value : Param_Value) return Result is
   begin
      Vst3_Log("Called Vst3.Controller.Set_Param_Normalised with: Id: " & Id'Image & "  and Value: " & Value'Image);
      -- NOTE(edg): Do we need to consider MIDI in here?

      if Id < Parameter_Type'Enum_Rep(Parameter_Type'First) or 
         Id > Parameter_Type'Enum_Rep(Parameter_Type'Last) then
         return False;
      end if;
      
      declare
         Parameter : Parameter_Type := Parameter_Type'Enum_Val(Id);
      begin
         This.Parameter_Cache(Parameter) := Normalised_Param_To_Plain(This, Id, Value);
      end;

      return Ok_True;
   end Set_Param_Normalised;

   function Set_Component_Handler (This : access Vst3_Controller; Handler : access System.Address) return Result is
   begin 
      Vst3_Log("Called Vst3.Controller.Set_Component_Handler");
      return Not_Implemented;
   end Set_Component_Handler;

   function Create_View (This : access Vst3_Controller; name : TUID) return access Vst3_View is
   begin
      Vst3_Log("Called Vst3.Controller.Create_View");
      -- TODO(edg): We'll need to copy in VST3-View here.
      return This.View'Access;
   end Create_View;

end Vst3.Controller;
