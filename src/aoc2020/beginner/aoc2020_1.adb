with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

package body Aoc2020_1 is

   procedure runAoc is

      package IO renames Ada.Text_IO;
      package INT_IO is new Ada.Text_IO.Integer_IO (Num => Integer);

      function calcHash (val : Integer) return Ada.Containers.Hash_Type is
      begin
         return Ada.Containers.Hash_Type (val);
      end calcHash;

      package NumSet is new Ada.Containers.Hashed_Sets (Element_Type => Integer,
                                                        Hash         => calcHash,
                                                        Equivalent_Elements => "=");

      all_set : NumSet.Set;

      file : IO.File_Type;
      file_name : constant String := "share/aoc2020_01.txt";

   begin

      all_set.Reserve_Capacity (200);

      IO.New_Line;
      IO.Put_Line ("Starting AOC2020 Day 1");

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop
         declare
            current_num : Integer;
         begin
            INT_IO.Get (file, current_num);

            all_set.Include (current_num);
         end;

      end loop;

      declare
         partA_found, partB_found : Boolean := False;
      begin

         for num of all_set loop

            declare
               other_needed : constant Integer := Integer (2020) - num;
            begin

               if not partA_found and then
                  all_set.Contains (other_needed)  then
                  IO.Put_Line ("Part A sum is" & Integer'Image
                               (other_needed * num));
                  partA_found := True;
               end if;

               if not partB_found then

                  for num2 of all_set loop
                     declare
                        third_needed : constant Integer := other_needed - num2;
                     begin
                        if third_needed > 0 and then
                           all_set.Contains (third_needed) then
                           IO.Put_Line ("Part B sum is" & Integer'Image
                                        (num * num2 * third_needed));
                           partB_found := True;
                           exit;
                        end if;
                     end;
                  end loop;

               end if;

               if partB_found and partA_found then
                  exit;
               end if;
            end;

         end loop;

      end;

      IO.Close (file);

   end runAoc;

end Aoc2020_1;
