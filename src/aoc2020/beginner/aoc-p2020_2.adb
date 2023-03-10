with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

package body Aoc.P2020_2 is

   procedure runAoc(input : Filename_Ptr) is

      package UB_IO renames Ada.Strings.Unbounded.Text_IO;
      package UB renames Ada.Strings.Unbounded;
      package INT_IO is new Ada.Text_IO.Integer_IO (Num => Integer);

      file : IO.File_Type;
      file_name : constant String := input.all;

      function getAllOccurances (char : Character; str : UB.Unbounded_String)
                                 return Integer is
      begin
         return UB.Count (str, "" & char);
      end getAllOccurances;

      sum_partA, sum_partB : Integer := 0;

   begin

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         declare
            lowest, highest : Integer;
            cur_char : Character;
            dummy_char : Character;
            cur_string : UB.Unbounded_String;

            occurances : Integer;
         begin
            INT_IO.Get (file, lowest);
            IO.Get (file, dummy_char);
            INT_IO.Get (file, highest);
            IO.Get (file, dummy_char);
            IO.Get (file, cur_char);
            IO.Get (file, dummy_char);
            IO.Get (file, dummy_char);
            UB_IO.Get_Line (file, cur_string);

            occurances := getAllOccurances (char => cur_char,
                                            str  => cur_string);
            if occurances >= lowest and then occurances <= highest then
               sum_partA := sum_partA + 1;
            end if;

            if UB.Element (cur_string, lowest) = cur_char xor
               UB.Element (cur_string, highest) = cur_char then

               sum_partB := sum_partB + 1;
            end if;

         end;
      end loop;

      IO.Put_Line ("Sum for Part A is" & sum_partA'Image);
      IO.Put_Line ("Sum for Part B is" & sum_partB'Image);

      IO.Close (file);

   end runAoc;

end Aoc.P2020_2;
