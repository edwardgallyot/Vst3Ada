with Ada.Text_IO; use Ada.Text_IO;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces; use Interfaces;

package Vst3 is 

   procedure Vst3_Log (Output : String);

   type C_String_32 is array (1 .. 32) of aliased char;
   type C_String_64 is array (1 .. 64) of aliased char;
   type C_String_128 is array (1 .. 128) of aliased char;
   type C_String_256 is array (1 .. 256) of aliased char;

   function To_C (Src : String) return C_String_32;
   function To_C (Src : String) return C_String_64;
   function To_C (Src : String) return C_String_128;
   function To_C (Src : String) return C_String_256;

   type C_Wide_String_128 is array (1 .. 128) of aliased char16_t;

   function To_C (Src : Wide_String) return C_Wide_String_128;
   function To_Ada (Src : C_Wide_String_128) return Wide_String;

   type TUID_Part is mod 2 ** 32;
   type TUID is array (1 .. 16) of aliased char;

   -- NOTE(edg): WIN32: This may not work on win32 see vst3_c_api.h:40
   function Make_TUID (One : Unsigned_32; Two : Unsigned_32; Three : Unsigned_32; Four : Unsigned_32) return TUID;

   subtype Param_Id is Unsigned; 
   subtype Param_Value is Long_Float;

   type Parameter_Flag_Option is (
      NoFlags,
      CanAutomate,
      IsReadOnly,
      IsWrapAround,
      IsList,
      IsHidden,
      IsProgramChange,
      IsBypass
   );

   type Parameter_Flag_Options is array(Parameter_Flag_Option) of Unsigned_32;
   Parameter_Flags : Parameter_Flag_Options := (
      NoFlags           => 0,
      CanAutomate       => 1,
      IsReadOnly        => Shift_Right(1, 1),
      IsWrapAround      => Shift_Right(1, 2),
      IsList            => Shift_Right(1, 3),
      IsHidden          => Shift_Right(1, 4),
      IsProgramChange   => Shift_Right(1, 15),
      IsBypass          => Shift_Right(1, 16)
   );

   type Parameter_Info is record
      Id : aliased Param_Id;
      Title : aliased C_Wide_String_128;
      Short_Title : aliased C_Wide_String_128;
      Units : aliased C_Wide_String_128; 
      Step_Count : aliased Int;  
      Default_Normalised_Value: aliased Param_Value;
      Unit_Id : aliased Int;
      Flags : aliased Int; 
   end record
   with Convention => C_Pass_By_Copy;  

   type Io_Modes is (Simple, Advanced, Offline_Processing) with Convention => C; 
   for Io_Modes use (Simple => 0, Advanced => 1, Offline_Processing => 2 );

   type Media_Types is (Audio, Event) with Convention => C;
   for Media_Types use (Audio => 0, Event => 1);

   type Bus_Directions is (Input, Output) with Convention => C;
   for Bus_Directions use (Input => 0, Output => 1);

   type Bus_Types is (Main, Aux) with Convention => C;
   for Bus_Types use (Main => 0, Aux => 1);

   type Bus_Info is record
      Media_Type     : aliased Media_Types;  
      Bus_Direction  : aliased Bus_Directions;  
      Channel_Count  : aliased Int;  
      Name           : aliased C_Wide_String_128;
      Bus_Type       : aliased Bus_Types;  
      Flags          : aliased Unsigned_32;
   end record
   with Convention => C_Pass_By_Copy;  

   type Routing_Info is record
      Media_Type  : aliased Media_Types; 
      Bus_Index   : aliased Int; 
      Channel     : aliased Int;  
   end record
   with Convention => C_Pass_By_Copy;  -- ./vst3_c_api.h:1704

   -- NOTE(edg): WIN32: This may not work see vst3_c_api.h:223
   type Result is (
      No_Interface,
      Ok_True,
      False,
      Invalid_Argument,
      Not_Implemented,
      Internal_Error,
      Not_Initialised,
      Out_Of_Memory
   ) with Convention => C;

   for Result use (
      No_Interface => -1,
      Ok_True => 0,
      False => 1,
      Invalid_Argument => 2,
      Not_Implemented => 3,
      Internal_Error => 4,
      Not_Initialised => 5,
      Out_Of_Memory => 6
   );

   type F_Unknown is record
      Query_Interface   : access function (This : access F_Unknown; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref           : access function (This : access F_Unknown) return Unsigned with Convention => C;
      Release           : access function (This : access F_Unknown) return Unsigned with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

end Vst3;
