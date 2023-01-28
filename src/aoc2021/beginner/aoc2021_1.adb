with Ada.Text_IO;

package body Aoc2021_1 is

   procedure runAoc is

      package IO renames Ada.Text_IO;
      package INT_IO is new Ada.Text_IO.Integer_IO (Num => Integer);

      file : IO.File_Type;
      file_name : constant String := "share/aoc2021_01.txt";

      running_count : Integer := 0;
      running_count_three : Integer := 0;
      previous_line : Integer := -1;
      current_line  : Integer := 0;

      procedure calcPartA is begin

         if previous_line /= -1 and then current_line > previous_line then
            running_count := running_count + 1;
         end if;

         previous_line := current_line;
      end calcPartA;

      type ringIndex is mod 3;
      ring_buffer : array (ringIndex) of Integer;
      ring_index : ringIndex := 0;

      previous_sum : Integer := 0;
      current_sum : Integer := 0;
      is_full : Boolean := False;

      procedure calcPartB is begin

         if is_full then

            current_sum := 0;
            for i of ring_buffer loop
               current_sum := current_sum + i;
            end loop;

         end if;

         if previous_sum /= 0 and then previous_sum < current_sum then

            running_count_three := running_count_three + 1;
         end if;

         previous_sum := current_sum;

         ring_buffer (ring_index) := current_line;

         if (not is_full) and then ring_index = 2 then
            is_full := True;

         end if;

         ring_index := ring_index + 1;

      end calcPartB;

   begin

      IO.New_Line;
      IO.Put_Line ("Starting AOC2021 Day 1.");

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         INT_IO.Get (file, current_line);

         calcPartA;
         calcPartB;

      end loop;

      calcPartB;

      IO.Put_Line ("Part A is" & running_count'Image);
      IO.Put_Line ("Part B is" & running_count_three'Image);

   end RunAoc;

end Aoc2021_1;
