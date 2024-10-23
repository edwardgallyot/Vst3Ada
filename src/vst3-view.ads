with System.Atomic_Counters; use System.Atomic_Counters;

package Vst3.View is  
   type Vst3_View;
   type Frame;

   View_Id : constant TUID := Make_TUID (16#5BC32507#, 16#D06049EA#, 16#A6151B52#, 16#2B755B29#);

   type Rectangle is record
      left     : aliased Integer_32;  
      top      : aliased Integer_32;  
      right    : aliased Integer_32;  
      bottom   : aliased Integer_32;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Frame_V_Table is record
      Query_Interface   : access function (This : access Frame; Interface_Id : TUID; Obj : access System.Address) return Result with Convention => C;  
      Add_Ref           : access function (This : access Frame) return Unsigned_32 with Convention => C;  
      Release           : access function (This : access Frame) return Unsigned_32 with Convention => C;  
      Resize_View       : access function (This : access Frame; View : access Vst3_View; Rect : access Rectangle) return Result with Convention => C;  
   end record
   with Convention => C_Pass_By_Copy;  

   type Frame is record
      V_Table : access Frame_V_Table;  
   end record
   with Convention => C_Pass_By_Copy;  

   type View_V_Table is record
      Query_Interface            : access function (This : access Vst3_View; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;  
      Add_Ref                    : access function (This : access Vst3_View) return Unsigned with Convention => C;  
      Release                    : access function (This : access Vst3_View) return Unsigned with Convention => C;  
      Is_Platform_Type_Supported : access function (This : access Vst3_View; Platform_Type : access TUID) return Result with Convention => C;  
      Attached                   : access function (This : access Vst3_View; Parent : System.Address; Platform_Type : TUID) return Result with Convention => C;  
      Removed                    : access function (This : access Vst3_View) return Result with Convention => C;  
      On_Wheel                   : access function (This : access Vst3_View; Distance : Float) return Result with Convention => C;  
      On_Key_Down                : access function (This : access Vst3_View; Key : Char16_T; Key_Code : Integer_16; Modifiers : Integer_16) return Result with Convention => C;
      On_Key_Up                  : access function (This : access Vst3_View; Key : Char16_T; Key_Code : Integer_16; Modifiers : Integer_16) return Result with Convention => C;
      Get_Size                   : access function (This : access Vst3_View; Size : access Rectangle) return Result with Convention => C;
      On_Size                    : access function (This : access Vst3_View; New_Size : access Rectangle) return Result with Convention => C;
      On_Focus                   : access function (This : access Vst3_View; State : C_Bool) return Result with Convention => C;
      Set_Frame                  : access function (This : access Vst3_View; New_Frame : access Frame) return Result with Convention => C;
      Can_Resize                 : access function (This : access Vst3_View) return Result with Convention => C;
      Check_Size_Constraint      : access function (This : access Vst3_View; Rect : access Rectangle) return Result with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;  

   function Query_Interface (This : access Vst3_View; Interface_Id : TUID; Obj : access Address) 
      return Result
      with Convention => C;

   function Add_Ref (This : access Vst3_View)
      return Unsigned
      with Convention => C;

   function Release (This : access Vst3_View) 
      return Unsigned
      with Convention => C;

   function Is_Platform_Type_Supported (This : access Vst3_View; Platform_Type : access TUID)
      return Result 
      with Convention => C;  

   function Attached (This : access Vst3_View; Parent : System.Address; Platform_Type : TUID)
      return Result 
      with Convention => C;  

   function Removed (This : access Vst3_View)
      return Result 
      with Convention => C;  
   
   function Check_Size_Constraint (This : access Vst3_View; Rect : access Rectangle)
      return Result 
      with Convention => C;

   function Can_Resize (This : access Vst3_View)
      return Result 
      with Convention => C;

   function Set_Frame (This : access Vst3_View; New_Frame : access Frame)
      return Result 
      with Convention => C;

   function On_Focus (This : access Vst3_View; State : C_Bool)
      return Result 
      with Convention => C;

   function On_Size (This : access Vst3_View; New_Size : access Rectangle)
      return Result 
      with Convention => C;

   function Get_Size (This : access Vst3_View; Size : access Rectangle)
      return Result 
      with Convention => C;

   function On_Key_Up (This : access Vst3_View; Key : Char16_T; Key_Code : Integer_16; Modifiers : Integer_16)
      return Result 
      with Convention => C;

   function On_Key_Down (This : access Vst3_View; Key : Char16_T; Key_Code : Integer_16; Modifiers : Integer_16)
      return Result 
      with Convention => C;

   function On_Wheel (This : access Vst3_View; Distance : Float)
      return Result 
      with Convention => C;  

   Table : aliased constant View_V_Table := (
      Query_Interface            => Query_Interface'Access,
      Add_Ref                    => Add_Ref'Access,
      Release                    => Release'Access,
      Is_Platform_Type_Supported => Is_Platform_Type_Supported'Access,
      Attached                   => Attached'Access,
      Removed                    => Removed'Access,
      Check_Size_Constraint      => Check_Size_Constraint'Access,
      Can_Resize                 => Can_Resize'Access,
      Set_Frame                  => Set_Frame'Access,
      On_Focus                   => On_Focus'Access,
      On_Size                    => On_Size'Access,
      Get_Size                   => Get_Size'Access,
      On_Key_Up                  => On_Key_Up'Access,
      On_Key_Down                => On_Key_Down'Access,
      On_Wheel                   => On_Wheel'Access
   );

   type Vst3_View is record
      V_Table     : access constant View_V_Table := Table'Access;  
      Ref_Count   : aliased Atomic_Unsigned := 0;
   end record
   with Convention => C_Pass_By_Copy;  

end Vst3.View;
