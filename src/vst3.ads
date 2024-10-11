with Ada.Text_IO;
with Interfaces.C;
with System;
with Interfaces.C;
with Interfaces.C.Strings;

package Vst3 is 
   use Interfaces.C;
   use Interfaces.C.Strings;
   use System;
   use Ada.Text_IO;

   type C_String is new chars_ptr;

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
      Query_Interface   : access function (This : access F_Unknown; Iid  : C_String; Obj  : Address) return Result with Convention => C;
      Add_Ref           : access function (This : access F_Unknown) return Unsigned with Convention => C;
      Release           : access function (This : access F_Unknown) return Unsigned with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

   type F_Unknown_Ref_Count is mod 2 ** Standard'Address_Size;
   type TUID is array (0 .. 15) of aliased char;
   procedure Say_Hi;
end Vst3;
