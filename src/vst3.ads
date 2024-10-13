with Ada.Text_IO;
with Interfaces.C;
with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces;

package Vst3 is 
   use Interfaces.C;
   use Interfaces.C.Strings;
   use Interfaces;
   use System;
   use Ada.Text_IO;

   -- NOTE(edg): WIN32: This may not work on win32 see vst3_c_api.h:40
   type TUID_Part is mod 2 ** 32;
   type TUID is array (1 .. 16) of aliased char;


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

   type Unknown is record
      Query_Interface   : access function (This : access Unknown; Interface_Id : TUID; Obj : Address) return Result with Convention => C;
      Add_Ref           : access function (This : access Unknown) return Unsigned with Convention => C;
      Release           : access function (This : access Unknown) return Unsigned with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

end Vst3;
