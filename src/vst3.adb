with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Vst3 is 
   procedure Say_Hi is 
      File : File_Type;
      Dir : String := Current_Directory;
   begin
      Put_Line (Standard_Output, "Lol");
   exception
      when others => 
            Create(File, Out_File, Dir & "/log.txt");
            Put_Line(File, "Cool");
            Close(File);
   end Say_Hi;
end Vst3;