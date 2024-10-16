with System.Atomic_Counters; use System.Atomic_Counters;
with Vst3; use Vst3;
with Vst3.Controller; use Vst3.Controller;
with Vst3.Plugin; use Vst3.Plugin;
with Ada.Unchecked_Conversion; 
with Vst3.Processor; use Vst3.Processor;

package body Vst3.Component is 

   type Vst3_Component_Ref is access all Vst3_Component;
   type Vst3_Plugin_Ref    is access all Vst3_Plugin;
   function To_Plugin is new Ada.Unchecked_Conversion(Vst3_Component_Ref, Vst3_Plugin_Ref);

   function Add_Ref (This : access Vst3_Component) return Unsigned is 
   begin
      Vst3_Log("Called Vst3.Component.Add_Ref");
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_Component) return Unsigned is
   begin
      Vst3_Log("Called Vst3.Component.Release");
      Decrement(This.Ref_Count);

      -- TODO(edg): De-allocate memory! The ordering is very important here,
      -- see cplug_vst3.c:1574.
      
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Vst3_Component;  Interface_Id: TUID; Obj : access Address) return Result is 
      Plugin : constant access Vst3_Plugin := To_Plugin(Vst3_Component_Ref(This));
   begin
      Vst3_Log("Called Vst3.Component.Query_Interface");

      if Interface_Id = Component_Id then
         Obj.all := Plugin.Component'Address;
      end if;

      if Interface_Id = Controller_Id then
         Obj.all := Plugin.Controller'Address;
      end if;

      if Interface_Id = Processor_Id then
         Obj.all := Plugin.Processor'Address;
      end if;

      return No_Interface;
   end Query_Interface;

   function Initialise (This : access Vst3_Component; Context : access F_Unknown) return Result is 
   begin
      Vst3_Log("Called Vst3.Component.Initialise");
      -- TODO(edg): Do we need to access the host here at all?
      -- TODO(edg): At this point we would init the user data.
      return Ok_True;
   end Initialise;

   function De_Initialise (This : access Vst3_Component) return Result is
   begin
      Vst3_Log("Called Vst3.Component.De_Initialise");
      -- TODO(edg): At this point we would destroy the user data.
      return Ok_True;
   end De_Initialise;

   function Get_Controller_Class_Id (This : access Vst3_Component; Class_Id : access TUID) return Result is
      Class_Id_To_Copy : constant TUID := Make_TUID(2, 0, 0, 0);
   begin
      Vst3_Log("Called Vst3.Component.Get_Controller_Class_Id");
      Class_Id.all := Class_Id_To_Copy;
      Vst3_Log("Class_Id is: " & Class_Id'Image);
      return Ok_True;
   end Get_Controller_Class_Id;

   function Set_Io_Mode (This : access Vst3_Component; Mode : IoModes) return Result  is
   begin
      Vst3_Log("Called Vst3.Component.Set_Io_Mode");
      return Not_Implemented;
   end Set_Io_Mode;

   function Get_Bus_Count (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions) return Int is
      Count : Int;
   begin
      Vst3_Log("Called Vst3.Component.Get_Bus_Count");
      case Media_Type is 
         when  Audio => 
            case Bus_Direction is
               when Input => Count := 1;
               when Output => Count := 1;
            end case;
         when Event => 
            case Bus_Direction is
               when Input => Count := 0;
               when Output => Count := 0;
            end case;
      end case;
      return Count;
   end Get_Bus_Count;

   function Get_Bus_Info (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; Info : access Bus_Info) return Result is
      Name : constant Wide_String := 
         (if Media_Type = Audio then 
            (if Bus_Direction = Input then
               "Audio Input" 
            else
               "Audio Output")
         else 
            (if Bus_Direction = Input then
               "Midi Input" 
            else
               "Midi Output"));
      Channel_Count : constant Int := (if Media_Type = Audio then 1 else 0);
      -- Default Flags
      Flags : constant Unsigned_32 := 1;
      Info_To_Copy : constant Bus_Info := (
         Name           => To_C (Name),
         Media_Type     => Media_Type,
         Bus_Direction  => Bus_Direction,
         Channel_Count  => Channel_Count,
         Bus_Type       => Main,
         Flags          => Flags
      );
   begin
      Vst3_Log ("Called Vst3.Component.Get_Bus_Info");
      Info.all := Info_To_Copy;
      return Ok_True;
   end Get_Bus_Info;

   function Get_Routing_Info (This : access Vst3_Component; Input_Info: access Routing_Info; Output_Info : access Routing_Info) return Result is
   begin
      Vst3_Log ("Called Vst3.Component.Get_Routing_Info");
      return Not_Implemented;
   end Get_Routing_Info;

   function Activate_Bus (This : access Vst3_Component; Media_Type : Media_Types; Bus_Direction : Bus_Directions; Index : Int; State : C_Bool) return Result is
   begin
      Vst3_Log ("Called Vst3.Component.Activate_Bus");
      if Index /= 0 then
         return Invalid_Argument;
      end if;
      return Ok_True;
   end Activate_Bus;

   function Set_Active (This : access Vst3_Component; State : C_Bool) return Result is
   begin
      Vst3_Log ("Called Vst3.Component.Set_Active");
      -- NOTE(edg): Do we actually care here? CPLUG doesn't
      return Ok_True;
   end Set_Active;

   function Set_State (This : access Vst3_Component; Stream : access Address) return Result is
   begin
      Vst3_Log ("Called Vst3.Component.Set_State");
      -- TODO(edg): Figure out how we handle streams
      return Not_Implemented;
   end Set_State;

   function Get_State (This : access Vst3_Component; Stream : access Address) return Result is
   begin
      Vst3_Log ("Called Vst3.Component.Get_State");
      -- TODO(edg): Figure out how we handle streams
      return Not_Implemented;
   end Get_State;

end Vst3.Component;

