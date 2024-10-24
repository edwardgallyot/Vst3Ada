with System.Atomic_Counters; use System.Atomic_Counters;

package Vst3.Factory is 

   type Vst3_Factory;

   Cardinality_Many_Instances : constant Int := 16#7FFFFFFF#;

   type Factory_Info is record
      Vendor   : aliased C_String_64;
      Url      : aliased C_String_256;
      Email    : aliased C_String_128; 
      Flags    : aliased Int; 
   end record
   with Convention => C_Pass_By_Copy;

   type Class_Info is record
      Cid         : aliased TUID := (others => nul);  
      Cardinality : aliased Int := 0;  
      Category    : aliased C_String_32 := (others => nul);  
      Name        : aliased C_String_64 := (others => nul);  
   end record
   with Convention => C_Pass_By_Copy;
   
   type Class_Info_2 is record
      Cid            : aliased TUID := (others => nul);  
      Cardinality    : aliased Int := 0;  
      Category       : aliased C_String_32 := (others => nul);  
      Name           : aliased C_String_64 := (others => nul);  
      Class_Flags    : aliased Unsigned := 0;  
      Sub_Categories : aliased C_String_128 := (others => nul);        
      Vendor         : aliased C_String_64 := (others => nul);  
      Version        : aliased C_String_64 := (others => nul);  
      Sdk_Version    : aliased C_String_64 := (others => nul);  
   end record
   with Convention => C_Pass_By_Copy;

   -- TODO(edg): Figure this one out...
   type Class_Info_W is record
      Cid         : aliased TUID := (others => nul);  
      Cardinality : aliased Int := 0;  
      Category    : aliased C_String_32 := (others => nul);  
      Name        : aliased C_String_64 := (others => nul);  
   end record
   with Convention => C_Pass_By_Copy;

   type Plugin_Factory_3_V_Table is record
      Query_Interface   : access function (This : access Vst3_Factory; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Add_Ref           : access function (This : access Vst3_Factory) return Unsigned with Convention => C;
      Release           : access function (This : access Vst3_Factory) return Unsigned with Convention => C;
      Get_Factory_Info  : access function (This : access Vst3_Factory; Info : access Factory_Info) return Result with Convention => C;
      Count_Classes     : access function (This : access Vst3_Factory) return Int with Convention => C;
      Get_Class_Info    : access function (This : access Vst3_Factory; Index : Int; Info: access Class_Info) return Result with Convention => C;
      Create_Instance   : access function (This : access Vst3_Factory; Class_Id : TUID; Interface_Id : TUID; Obj : access Address) return Result with Convention => C;
      Get_Class_Info_2  : access function (This : access Vst3_Factory; Index : Int; Info : access Class_Info_2) return Result with Convention => C;
      Get_Class_Info_W  : access function (This : access Vst3_Factory; Index : Int; Info : access Class_Info_W) return Result with Convention => C;
      Set_Host_Context  : access function (This : access Vst3_Factory; Context : access F_Unknown) return Result with Convention => C;
   end record
   with Convention => C_Pass_By_Copy;

   Plugin_Factory_3_Id : constant TUID := Make_TUID (16#4555A2AB#, 16#C1234E57#, 16#9B122910#, 16#36878931#);
   Plugin_Factory_2_Id : constant TUID := Make_TUID (16#0007B650#, 16#F24B4C0B#, 16#A464EDB9#, 16#F00B2ABB#);
   Plugin_Factory_Id   : constant TUID := Make_TUID (16#7A4D811C#, 16#52114A1F#, 16#AED9D2EE#, 16#0B43BF9F#);

   function Add_Ref (this : access Vst3_Factory)
      return Unsigned
      with Convention => C;

   function Release (this : access Vst3_Factory) 
      return Unsigned
      with Convention => C;

   function Query_Interface (This : access Vst3_Factory; Interface_Id : TUID; Obj : access Address) 
      return Result
      with Convention => C;

   function Get_Factory_Info (This : access Vst3_Factory; Info : access Factory_Info)
      return Result
      with Convention => C;

   function Count_Classes (This : access Vst3_Factory)
      return Int
      with Convention => C;

   function Get_Class_Info (This : access Vst3_Factory; Index : Int; Info: access Class_Info)
      return Result
      with Convention => C;

   function Create_Instance (This : access Vst3_Factory; Class_Id : TUID; Interface_Id : TUID; Obj : access Address)
      return Result
      with Convention => C;

   function Get_Class_Info_2 (This : access Vst3_Factory; Index : Int; Info : access Class_Info_2) 
      return Result 
      with Convention => C;

   function Get_Class_Info_W (This : access Vst3_Factory; Index : Int; Info : access Class_Info_W) 
      return Result 
      with Convention => C;

   function Set_Host_Context (This : access Vst3_Factory; Context : access F_Unknown) 
      return Result 
      with Convention => C;

   Table : aliased constant Plugin_Factory_3_V_Table := (
      Query_Interface   => Query_Interface'Access,
      Release           => Release'Access,
      Add_Ref           => Add_Ref'Access,
      Get_Factory_Info  => Get_Factory_Info'Access,
      Count_Classes     => Count_Classes'Access,
      Get_Class_Info    => Get_Class_Info'Access,
      Create_Instance   => Create_Instance'Access,
      Get_Class_Info_2  => Get_Class_Info_2'Access,
      Get_Class_Info_W  => Get_Class_Info_W'Access,
      Set_Host_Context  => Set_Host_Context'Access
   );

   type Vst3_Factory is record
      V_Table: access constant Plugin_Factory_3_V_Table := Table'Access;
      Ref_Count: aliased Atomic_Unsigned := 0;
   end record 
   with Convention => C_Pass_By_Copy;

end Vst3.Factory;
