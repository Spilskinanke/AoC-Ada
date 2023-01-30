with Ada.Text_IO;

package body Aoc2022_4 is

   procedure runAoc is

      package IO renames Ada.Text_IO;
      package INT_IO is new Ada.Text_IO.Integer_IO (Num => Integer);

      file : IO.File_Type;
      file_name : constant String := "share/aoc2022_04.txt";

      sum_partA, sum_partB : Integer := 0;

      function calcTotalOverlap (lower1, upper1,
                                 lower2, upper2 : Integer) return Boolean is
         lower_diff : constant Integer := lower1 - lower2;
         upper_diff : constant Integer := upper1 - upper2;
      begin
         return
            lower_diff = 0 or else
            upper_diff = 0 or else
            (lower_diff < 0 xor upper_diff < 0);

      end calcTotalOverlap;

      function calcPartialOverlap (lower1, upper1,
                                   lower2, upper2 : Integer) return Boolean is
         l1_u2_diff : constant Integer := lower1 - upper2;
         u1_l2_diff : constant Integer := upper1 - lower2;
      begin
         return 
            l1_u2_diff = 0 or else
            u1_l2_diff = 0 or else
            (u1_l2_diff < 0 xor l1_u2_diff < 0);
      end calcPartialOverlap;

   begin

      IO.New_Line;
      IO.Put_Line ("Starting AOC2022 Day 4.");

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         declare
            lower_bound_1, higher_bound_1,
            lower_bound_2, higher_bound_2 : Integer;
            dummy_char : Character;
         begin

            INT_IO.Get (file, lower_bound_1);
            IO.Get (file, dummy_char);
            INT_IO.Get (file, higher_bound_1);
            IO.Get (file, dummy_char);
            INT_IO.Get (file, lower_bound_2);
            IO.Get (file, dummy_char);
            INT_IO.Get (file, higher_bound_2);

            if calcTotalOverlap (lower_bound_1, higher_bound_1,
                                 lower_bound_2, higher_bound_2) then
                        sum_partA := sum_partA + 1;
            end if;

            if calcPartialOverlap (lower_bound_1, higher_bound_1,
                                   lower_bound_2, higher_bound_2) then
               sum_partB := sum_partB + 1;
            end if;

         end;

      end loop;

      IO.Put_Line ("Sum for Part A is" & sum_partA'Image);
      IO.Put_Line ("Sum for Part B is" & sum_partB'Image);
      
      IO.Close(file);

   end runAoc;

end Aoc2022_4;
