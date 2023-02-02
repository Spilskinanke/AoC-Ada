with Ada.Text_IO;

package body Aoc.P2022_2 is

   procedure runAoc(input : Filename_Ptr) is

      file : IO.File_Type;
      file_name : constant String := input.all;

      line : String := "___";

      type PlayerChoice is mod 3;
      elf, you : PlayerChoice;

      type UnsignedInt is mod 2 ** 31;
      part_a_sum, part_b_sum : UnsignedInt := 0;

      procedure calcPartA is
         round_points : UnsignedInt := 0;
      begin

         if elf = you then
            round_points := 3;
         elsif (elf < you) /=
         ((UnsignedInt (elf) + UnsignedInt (you)) = 2)
         then
            round_points := 6;
         end if;

         part_a_sum := part_a_sum + round_points + ((UnsignedInt (you) + 1));

      end calcPartA;

      procedure calcPartB is begin

         part_b_sum := part_b_sum + UnsignedInt (elf + (you - 1)) + 1 +
            UnsignedInt (you) * 3;

      end calcPartB;

   begin
      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         line := IO.Get_Line (file);

         elf := PlayerChoice (Character'Pos (line (line'First)) - 65);
         you := PlayerChoice (Character'Pos (line (line'Last)) - 88);

         calcPartA;
         calcPartB;

      end loop;

      IO.Put_Line ("Part A is " & UnsignedInt'Image (part_a_sum));
      IO.Put_Line ("Part B is " & UnsignedInt'Image (part_b_sum));

      IO.Close (file);

   end RunAoc;

end Aoc.P2022_2;
