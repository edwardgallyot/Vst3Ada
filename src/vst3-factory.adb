with Ada.Unchecked_Conversion;
with System.Atomic_Counters; use System.Atomic_Counters;
with Vst3.Plugin; use Vst3.Plugin;

package body Vst3.Factory is 

   function To_C (Src: String) return C_String_32 is
      Res : C_String_32 := (others => nul);
   begin
      for I in Src'Range loop Res (I) := To_C (Src (I)); end loop;
      return Res;
   end To_C;

   function To_C (Src: String) return C_String_64 is
      Res : C_String_64 := (others => nul);
   begin
      for I in Src'Range loop Res (I) := To_C (Src (I)); end loop;
      return Res;
   end To_C;

   function To_C (Src: String) return C_String_128 is
      Res : C_String_128 := (others => nul);
   begin
      for I in Src'Range loop Res (I) := To_C (Src (I)); end loop;
      return Res;
   end To_C;

   function To_C (Src: String) return C_String_256 is
      Res : C_String_256 := (others => nul);
   begin
      for I in Src'Range loop Res (I) := To_C (Src (I)); end loop;
      return Res;
   end To_C;

   function Make_Factory_Info (Vendor : String; Url : String; Email : String; Flags : Int) return Factory_Info is 
      Info : Factory_Info := (Flags => Flags, Vendor => To_C (Vendor), Email => To_C (Email), Url => To_C (Url));
   begin
      return Info;
   end Make_Factory_Info;

   function Make_Class_Info (Cid : TUID; Cardinality : Int; Category : String; Name : String) return Class_Info is
      Info : Class_Info := (
         Cid => Cid,
         Cardinality => Cardinality,
         Name => To_C (Name),
         Category => To_C (Category));
   begin
      return Info;
   end Make_Class_Info;

   function Make_Class_Info_2 (
      Cid            : TUID;
      Cardinality    : Int;
      Category       : String;
      Name           : String;
      Class_Flags    : Unsigned;  
      Sub_Categories : String;
      Vendor         : String;  
      Version        : String;  
      Sdk_Version    : String 
   ) return Class_Info_2 is
      Info_2 : Class_Info_2 := (
         Cid => Cid,
         Cardinality => Cardinality,
         Category => To_C (Category),
         Name => To_C (Name),
         Class_Flags => Class_Flags,
         Sub_Categories => To_C (Sub_Categories),
         Vendor => To_C (Vendor),
         Version => To_C (Version),
         Sdk_Version => To_C (Sdk_Version));
   begin
      return Info_2;
   end Make_Class_Info_2;

   function Add_Ref (This : access Plugin_Factory) return Unsigned is 
   begin
      Put_Line("Add Ref");
      Increment (This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Add_Ref;

   function Release (This : access Plugin_Factory) return Unsigned is 
   begin
      Put_Line("Release");
      Decrement (This.Ref_Count);
      return Unsigned(This.Ref_Count);
   end Release;

   function Query_Interface (This : access Plugin_Factory; iid : TUID; obj : access Address) return Result is
   begin
      Put_Line("Query Interface");
      if iid = I_Plugin_Factory_3_IID or
         iid = I_Plugin_Factory_2_IID or
         iid = I_Plugin_Factory_IID   or 
         iid = F_Unknown_IID          then
         obj.all := This.all'Address;
         return Ok_True;
      end if;

      return No_Interface;
   end Query_Interface;

   function Count_Classes (This : access Plugin_Factory) return Int is
   begin
      Put_Line("Count Classes");
      return 1;
   end Count_Classes;

   function Get_Factory_Info (This : access Plugin_Factory;
                              Info : access Factory_Info) return Result is
   begin
      Info.all := Make_Factory_Info ("the bois", "www.google.com","edgallyot@gmail.com", 0);
      return Ok_True;
   end Get_Factory_Info;

   function Get_Class_Info (This : access Plugin_Factory; Index : Int; Info: access Class_Info) return Result is
      Class_Id : constant TUID := Make_TUID(1, 0, 0, 0);
      Info_To_Copy : constant Class_Info := Make_Class_Info(Class_Id, Cardinality_Many_Instances, "Audio Module Class", "Sami");
   begin
      Info.all := Info_To_Copy;
      return Ok_True;
   end Get_Class_Info;


   function Create_Instance (This : access Plugin_Factory; Class_Id : TUID; Interface_Id : TUID; Obj : access Address) return Result is 
      Class_TUID : constant TUID := Make_TUID(1, 0, 0, 0);
      Instance : access Component;
      Ignore : Unsigned;
   begin
      Put_Line("Create Instance");
      if Class_Id = Class_TUID then 
         if Interface_Id = F_Unknown_IID or Interface_Id = I_Component_IID then
            -- TODO(edg): This should be a "Plugin" not a standalone Component, we'll get there tho.
            Instance := new Component;
            Ignore   := Vst3.Plugin.Add_Ref (Instance);
            Obj.all  := Instance.all'Address;
            return Ok_True;
         end if; 
      end if;
      return No_Interface;
   end Create_Instance;

   function Get_Class_Info_2 (This : access Plugin_Factory; Index : Int; Info : access Class_Info_2) return Result is
      Cid : constant TUID := Make_TUID(1, 0, 0, 0);
      Component_Simple_Mode_Supported : constant Unsigned_32 := Shift_Right(1, 1);
      function To_Unsigned is new Ada.Unchecked_Conversion(Unsigned_32, Unsigned);
      Info_2 : Class_Info_2 := Make_Class_Info_2(
         Cid,
         Cardinality_Many_Instances,
         "Audio Module Class",
         "Sami",
         To_Unsigned(Component_Simple_Mode_Supported),
         "Fx",
         "The bois",
         "0.0.1",
         "VST 3.7.9"
      );
   begin
      Info.all := Info_2; 
      return Ok_True;
   end Get_Class_Info_2;

   function Get_Class_Info_W (This : access Plugin_Factory; Index : Int; Info : access Class_Info_W) return Result is
   begin
      Put_Line("Get Class Info W");
      -- TODO(edg): We should implement this. Right now it's easier to ignore it to get things up and running.
      return Not_Implemented;
   end Get_Class_Info_W;

   function Set_Host_Context (This : access Plugin_Factory; Context : access Unknown) return Result is
   begin
      Put_Line("Set Host Context");
      -- NOTE(edg): Unused
      return Ok_True;
   end Set_Host_Context;

end Vst3.Factory;

