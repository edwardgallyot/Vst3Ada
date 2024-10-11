package body Vst3.I_Plugin_Factory_3 is 

   function Add_Ref(This : access I_Plugin_Factory_3) return Unsigned is 
   begin
      Put_Line("Add Ref");
      This.Ref_Count := This.Ref_Count + 1;
      return 0;
   end Add_Ref;

   function Release(This : access I_Plugin_Factory_3) return Unsigned is 
   begin
      Put_Line("Release");
      return 0;
   end Release;

   function Query_Interface(This : access I_Plugin_Factory_3; iid  : C_String; obj  : Address) return Result is
   begin
      Put_Line("Query Interface");
      return Ok_True;
   end Query_Interface;

   function Count_Classes(This : access I_Plugin_Factory_3) return Int is
   begin
      return 1;
   end Count_Classes;

   function Get_Factory_Info(This : access I_Plugin_Factory_3; Info : access P_Factory_Info) return Result is
   begin
      return Ok_True;
   end Get_Factory_Info;

   function Get_Class_Info (This : access I_Plugin_Factory_3; Index : Int; Info: access P_Class_Info) return Result is
   begin
      return Ok_True;
   end Get_Class_Info;

   function Create_Instance (This : access I_Plugin_Factory_3; Cid : C_String; Iid : C_String; Obj : Address) return Result is 
   begin
      return Ok_True;
   end Create_Instance;

   function Get_Class_Info_2 (This : access I_Plugin_Factory_3; Index : Int; Info : access P_Class_Info_2) return Result is
   begin
      return Ok_True;
   end Get_Class_Info_2;

   function Get_Class_Info_W (This : access I_Plugin_Factory_3; Index : Int; Info : access P_Class_Info_W) return Result is
   begin
      return Ok_True;
   end Get_Class_Info_W;

   function Set_Host_Context (This : access I_Plugin_Factory_3; Context : access F_Unknown) return Result is
   begin
      return Ok_True;
   end Set_Host_Context;

end Vst3.I_Plugin_Factory_3;

