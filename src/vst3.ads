with Ada.Text_IO; use Ada.Text_IO;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces; use Interfaces;

package Vst3 is 

   type C_String_32 is array (1 .. 32) of aliased char;
   type C_String_64 is array (1 .. 64) of aliased char;
   type C_String_128 is array (1 .. 128) of aliased char;
   type C_String_256 is array (1 .. 256) of aliased char;

   function To_C (Src : String) return C_String_32;
   function To_C (Src : String) return C_String_64;
   function To_C (Src : String) return C_String_128;
   function To_C (Src : String) return C_String_256;

   type TUID_Part is mod 2 ** 32;
   type TUID is array (1 .. 16) of aliased char;

   -- NOTE(edg): WIN32: This may not work on win32 see vst3_c_api.h:40
   function Make_TUID (One : Unsigned_32; Two : Unsigned_32; Three : Unsigned_32; Four : Unsigned_32) return TUID;


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
