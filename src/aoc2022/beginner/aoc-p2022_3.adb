with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Aoc.P2022_3 is

   procedure runAoc(input : Filename_Ptr) is

      file : IO.File_Type;
      file_name : constant String := input.all;

      function calcHash (val : Character) return Ada.Containers.Hash_Type is
      begin
         return Ada.Containers.Hash_Type (Character'Pos (val));
      end calcHash;

      package NumSet is new Ada.Containers.Hashed_Sets (Element_Type => Character,
                                                        Hash         => calcHash,
                                                        Equivalent_Elements => "=");

      sum_partA, sum_partB : Integer := 0;

      function calcWorth (char : Character) return Integer is begin
         if Is_Lower (char) then
            return Character'Pos (To_Upper (char)) - 64;
         end if;
            return Character'Pos (To_Lower (char)) - 70;
      end calcWorth;

      function buildSet (str : String) return NumSet.Set is
         set : NumSet.Set;
      begin
         for cur_char of str loop
            set.Include (cur_char);
         end loop;
         return set;
      end buildSet;

      procedure buildSet (set : in out NumSet.Set; str : String) is
      begin
         for cur_char of str loop
            set.Include (cur_char);
         end loop;
      end buildSet;

      procedure calcPartA (line : String) is
         middle : constant Integer := line'Length / 2;
         first : constant String := line (line'First .. middle);
         last : constant String := line (middle + 1 .. line'Last);
         last_set : constant NumSet.Set := buildSet (last);
      begin

         for char of first loop

            if last_set.Contains (char) then

               sum_partA := sum_partA + calcWorth (char);
               exit;
            end if;
         end loop;
      end calcPartA;

      type ArrIndex is mod 3;
      arr_cursor : ArrIndex := 0;
      lines : array (ArrIndex) of NumSet.Set := (NumSet.Empty_Set,
                                                 NumSet.Empty_Set,
                                                 NumSet.Empty_Set);

      procedure calcPartB (line : String) is begin
         
         lines (arr_cursor).Clear;
         buildSet (lines (arr_cursor), line);
         arr_cursor := arr_cursor + 1;

         if arr_cursor = 0 then -- do check
            declare
               temp_set : NumSet.Set := lines (lines'First);
            begin
               temp_set.Intersection (lines (lines'First + 1));
               temp_set.Intersection (lines (lines'Last));
               sum_partB := sum_partB + calcWorth
                  (temp_set.Constant_Reference (temp_set.First).Element.all);
            end;
         end if;

      end calcPartB;

   begin

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         declare
            line : constant String := IO.Get_Line (file);
         begin
            calcPartA (line);
            calcPartB (line);
         end;

      end loop;

      IO.Put_Line ("Sum for Part A is " & sum_partA'Image);
      IO.Put_Line ("Sum for Part B is " & sum_partB'Image);
      
      IO.Close (file);

   end runAoc;

end Aoc.P2022_3;
