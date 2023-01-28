with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Unbounded;

package body Aoc2022_1 is

   procedure runAoc is

      package IO renames Ada.Strings.Unbounded.Text_IO;
      package UB renames Ada.Strings.Unbounded;

      file : File_Type;
      file_name : constant String := "share/aoc2022_01.txt";
      current_line : UB.Unbounded_String;
      three_highest_sums : array (0 .. 2) of Integer := (0, 0, 0);
      current_sum : Integer := 0;

      procedure checkIfHighest is

         function getMinimumIndex return Integer is
            min : Integer := Integer'Last;
            index : Integer := 0;
         begin

            for I in three_highest_sums'Range loop
               if three_highest_sums (I) < min then
                  min := three_highest_sums (I);
                  index := I;
               end if;
            end loop;

            return index;
         end getMinimumIndex;

         min_index : constant Integer := getMinimumIndex;
      begin
         if three_highest_sums (min_index) < current_sum then
            three_highest_sums (min_index) := current_sum;
         end if;
      end checkIfHighest;

   begin

      New_Line;
      Put_Line ("Starting AOC2022 Day 1.");

      Open (file, In_File, file_name);

      while not End_Of_File (file) loop
         current_line := IO.Get_Line (file);

         if UB.Length (current_line) = 0 then
            checkIfHighest;
            current_sum := 0;
         else
            current_sum :=
            current_sum + Integer'Value (UB.To_String (current_line));
         end if;

      end loop;

      checkIfHighest;
      current_sum := 0;

      for I in three_highest_sums'Range loop
         current_sum := current_sum + three_highest_sums (I);
         Put_Line ("the " & Integer'Image (I) & "th biggest is "
                   & Integer'Image (three_highest_sums (I)));
      end loop;

      Put_Line ("Total Sum is" & Integer'Image (current_sum));

      Close (file);

   end RunAoc;

end Aoc2022_1;
