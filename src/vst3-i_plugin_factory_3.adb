
with Vst3.I_Plugin_Factory_3;
package body Vst3.I_Plugin_Factory_3 is 

   function Make_Factory_Info (Vendor : String; Url : String; Email : String; Flags : Int) return P_Factory_Info is 
      Factory_Info : P_Factory_Info := (Flags => Flags, Vendor => (others => nul), Email => (others => nul), Url => (others => nul));
   begin
      for I in Vendor'Range loop Factory_Info.Vendor (I) := To_C (Vendor (I)); end loop;
      for I in Email'Range loop Factory_Info.Email (I) := To_C (Email (I)); end loop;
      for I in Url'Range loop Factory_Info.Url (I) := To_C (Url (I)); end loop;
      return Factory_Info;
   end Make_Factory_Info;

   function Add_Ref (This : access I_Plugin_Factory_3) return Unsigned is 
   begin
      Put_Line("Add Ref");
      This.Ref_Count := This.Ref_Count + 1;
      return 0;
   end Add_Ref;

   function Release (This : access I_Plugin_Factory_3) return Unsigned is 
   begin
      Put_Line("Release");
      return 0;
   end Release;

   function Query_Interface (This : access I_Plugin_Factory_3; iid  : C_String; obj  : Address) return Result is
   begin
      Put_Line("Query Interface");
      return Ok_True;
   end Query_Interface;

   function Count_Classes (This : access I_Plugin_Factory_3) return Int is
   begin
      Put_Line("Count Classes");
      return 1;
   end Count_Classes;

   function Get_Factory_Info (This : access I_Plugin_Factory_3;
                              Info : access P_Factory_Info) return Result is
   begin
      Info.all := Make_Factory_Info ("the bois", "www.google.com","edgallyot@gmail.com", 0);
      return Ok_True;
   end Get_Factory_Info;

   function Get_Class_Info (This : access I_Plugin_Factory_3; Index : Int; Info: access P_Class_Info) return Result is
   begin
      Put_Line("Get Class Info");
      return Ok_True;
   end Get_Class_Info;

   function Create_Instance (This : access I_Plugin_Factory_3; Cid : C_String; Iid : C_String; Obj : Address) return Result is 
   begin
      Put_Line("Create Instance");
      return Ok_True;
   end Create_Instance;

   function Get_Class_Info_2 (This : access I_Plugin_Factory_3; Index : Int; Info : access P_Class_Info_2) return Result is
   begin
      Put_Line("Get Class Info 2");
      return Ok_True;
   end Get_Class_Info_2;

   function Get_Class_Info_W (This : access I_Plugin_Factory_3; Index : Int; Info : access P_Class_Info_W) return Result is
   begin
      Put_Line("Get Class Info W");
      return Ok_True;
   end Get_Class_Info_W;

   function Set_Host_Context (This : access I_Plugin_Factory_3; Context : access F_Unknown) return Result is
   begin
      Put_Line("Set Host Context");
      return Ok_True;
   end Set_Host_Context;

end Vst3.I_Plugin_Factory_3;

