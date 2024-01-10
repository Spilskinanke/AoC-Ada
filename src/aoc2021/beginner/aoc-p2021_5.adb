package body Aoc.P2021_5 is

   procedure runAoc (input : Filename_Ptr) is

      file : IO.File_Type;
      file_name : constant String := input.all;

      type SLine_Direction is (Vertical, Horizontal);
      type SLine (dir : SLine_Direction) is record
         x, y, length : Natural;
      end record;



   begin

   end runAoc;

end Aoc.P2021_5;
