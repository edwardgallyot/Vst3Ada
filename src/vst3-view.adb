with Vst3.Constants; use Vst3.Constants;

package body Vst3.View is  

   -- TODO(edg): We want there to be seperate source directories once we go cross platform
   X11_Window_Id : constant TUID := "X11EmbedWindowID";

   function Query_Interface (This : access Vst3_View; Interface_Id : TUID; Obj : access Address)  return Result is
      Unused : Unsigned;
   begin
      Vst3_Log ("Calling Vst3.View.Query_Interface");

      if Interface_Id = View_Id or 
         Interface_Id = Unknown_Id then
         Vst3_Log("Found Interface" & Interface_Id'Image);
         Obj.all := This.all'Address;
         Unused := Add_Ref (This);
         return Ok_True;
      end if;

      return No_Interface;
   end Query_Interface;

   function Add_Ref (This : access Vst3_View) return Unsigned is
   begin
      Vst3_Log ("Calling Vst3.View.Add_Ref");
      Increment(This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Vst3_View)  return Unsigned is
   begin
      Vst3_Log ("Calling Vst3.View.Release");
      Decrement(This.Ref_Count);
      -- TODO(edg): We need to deallocate the memory here but not for now.
      return Unsigned(This.Ref_Count);
   end Release;

   function Is_Platform_Type_Supported (This : access Vst3_View; Platform_Type : access TUID) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Is_Platform_Type_Supported");

      if Platform_Type.all = X11_Window_Id then
         return Ok_True;
      end if;

      return Invalid_Argument;
   end Is_Platform_Type_Supported;

   function Attached (This : access Vst3_View; Parent : System.Address; Platform_Type : TUID) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Attached");
      return Ok_True;
   end Attached;

   function Removed (This : access Vst3_View) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Removed");
      return Ok_True;
   end Removed;

   function Check_Size_Constraint (This : access Vst3_View; Rect : access Rectangle) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Check_Size_Constraint");
      return Ok_True;
   end Check_Size_Constraint;

   function Can_Resize (This : access Vst3_View) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Can_Resize");
      return Ok_True;
   end Can_Resize;

   function Set_Frame (This : access Vst3_View; New_Frame : access Frame) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Set_Frame");
      return Ok_True;
   end Set_Frame;

   function On_Focus (This : access Vst3_View; State : C_Bool) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.On_Focus");
      return Ok_True;
   end On_Focus;

   function On_Size (This : access Vst3_View; New_Size : access Rectangle) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.On_Size");
      return Ok_True;
   end On_Size;

   function Get_Size (This : access Vst3_View; Size : access Rectangle) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.Get_Size");
      return Ok_True;
   end Get_Size;

   function On_Key_Up (This : access Vst3_View; Key : Char16_T; Key_Code : Integer_16; Modifiers : Integer_16) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.On_Key_Up");
      return Ok_True;
   end On_Key_Up;

   function On_Key_Down (This : access Vst3_View; Key : Char16_T; Key_Code : Integer_16; Modifiers : Integer_16) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.On_Key_Down");
      return Ok_True;
   end On_Key_Down;

   function On_Wheel (This : access Vst3_View; Distance : Float) return Result is
   begin
      Vst3_Log ("Calling Vst3.View.On_Wheel");
      return Ok_True;
   end On_Wheel;
end Vst3.View;
