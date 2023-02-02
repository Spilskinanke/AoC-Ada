with Ada.Unchecked_Conversion;

package body Aoc.P2021_3 is
   procedure runAoc (input : Filename_Ptr) is

      file, fileB : IO.File_Type;
      file_name : constant String := input.all;

      type BitMatrix is array (Positive range <>, Positive range <>) of Boolean
         with Component_Size => 1;

      type BitVector is array (Positive range <>) of Boolean
         with Component_Size => 1;

      function accumulate (x : Natural) return Integer
         with Pre => x in 0 | 1 is
      begin
         return (x * 2) - 1;
      end accumulate;

   begin

      IO.Open (file, IO.In_File, file_name);
      declare
         line : constant String := IO.Get_Line (file);
         type GammaTracker is array (Positive range <>) of Integer;
         type BoundedBitVector is new BitVector (line'Range)
            with Component_Size => 1;

         tracker : GammaTracker (line'Range) := [others => 0];
         binary_res : BoundedBitVector;
         low_bit_vector, high_bit_vector : BoundedBitVector;

         function Convert is new Ada.Unchecked_Conversion (Source => BoundedBitVector,
                                                           Target => Natural);

         firstNum, secondNum, firstNumB, secondNumB,
         total_lines : Natural := 0;

         procedure tally (line : String) is begin
            for I in line'Range loop
               tracker (tracker'Last - (I - tracker'First)) :=
                  tracker (tracker'Last - (I - tracker'First))
                     + accumulate (Natural'Value ("" & line (I)));

               binary_res (tracker'Last - (I - tracker'First)) :=
                  tracker (tracker'Last - (I - tracker'First)) > 0;

            end loop;
            total_lines := total_lines + 1;
         end tally;

--------------------------------------------------------------------------------

         procedure recurseOnArray (M                  : BitMatrix;
                                   first_0s, first_1s : Natural;
                                   line_length        : Positive;
                                   depth              : Positive;
                                   more_common        : Boolean) is

            zeroes                             : BitMatrix (1 .. first_0s, 1 .. line_length);
            ones                               : BitMatrix (1 .. first_1s, 1 .. line_length);
            zeroes_count_next,
            ones_count_next                    : Natural := 0;
            zeroes_inc, ones_inc               : Positive := 1;

            more_common_bit                    : constant Boolean :=
                                                   (if more_common then first_1s >= first_0s
                                                    else first_1s < first_0s);
         begin

            if M'Length (1) = 1 then
               for I in M'Range (2) loop
                  if more_common then
                     high_bit_vector (I) := M (1, I);
                  else
                     low_bit_vector (I) := M (1, I);
                  end if;
               end loop;
               return;
            end if;

            for I in M'Range (1) loop
               if M (I, depth) = False and then not more_common_bit then -- found a zero at index J
                  for J in M'Range (2) loop
                     zeroes (zeroes_inc, J) := M (I, J); -- put whole row into zeroes
                  end loop;

                  if depth /= line_length and then zeroes (zeroes_inc, depth + 1) then
                     ones_count_next := ones_count_next + 1;
                  else
                     zeroes_count_next := zeroes_count_next + 1;
                  end if;

                  zeroes_inc := zeroes_inc + 1; -- increment array holder
               elsif M (I, depth) and then more_common_bit then
                  for J in M'Range (2) loop
                     ones (ones_inc, J) := M (I, J); -- put whole row into zeroes
                     null;
                  end loop;

                  if depth /= line_length and then ones (ones_inc, depth + 1) then
                     ones_count_next := ones_count_next + 1;
                  else
                     zeroes_count_next := zeroes_count_next + 1;
                  end if;

                  ones_inc := ones_inc + 1; -- increment array holder

               end if;
            end loop;

            if more_common_bit then
               recurseOnArray (ones, zeroes_count_next, ones_count_next, line_length, depth + 1, more_common);
            else
               recurseOnArray (zeroes, zeroes_count_next, ones_count_next, line_length, depth + 1, more_common);
            end if;

         end recurseOnArray;

         procedure calcPartB (total, line_length : Natural) is
            M : BitMatrix (1 .. total, 1 .. line_length);
            I : Positive := 1;
            first_0s, first_1s : Natural := 0;

            procedure loadM (line : String) is begin
               for I_line in line'Range loop
                  if line (I_line) = '0' then
                     M (I, I_line) := False;
                  else
                     M (I, I_line) := True;
                  end if;
               end loop;
               I := I + 1;
               if line (line'First) = '0' then
                  first_0s := first_0s + 1;
               else
                  first_1s := first_1s + 1;
               end if;

            end loadM;

         begin
            IO.Open (fileB, IO.In_File, file_name);
            while not IO.End_Of_File (fileB) loop
               declare
                  line : constant String := IO.Get_Line (fileB);
               begin
                  loadM (line);
               end;
            end loop;

            recurseOnArray (M, first_0s, first_1s, line_length, 1, True);
            recurseOnArray (M, first_0s, first_1s, line_length, 1, False);

         end calcPartB;

      begin

         tally (line);

         while not IO.End_Of_File (file) loop
            declare
               line : constant String := IO.Get_Line (file);
            begin
               tally (line);
            end;
         end loop;

         firstNum := Convert (binary_res);
         for I in binary_res'Range loop
            binary_res (I) := not binary_res (I);
         end loop;
         secondNum := Convert (binary_res);

         IO.Put_Line ("Part A sum is" & Natural'Image (firstNum * secondNum));

         IO.Close (file);

         calcPartB (total_lines, line'Length);

         declare
            uh : Natural := 1;
         begin

            for Bool of reverse high_bit_vector loop
               if Bool then
                  firstNumB := firstNumB + (2 ** (uh - 1));
               end if;
               uh := uh + 1;
            end loop;

            uh := 1;

            for Bool of reverse low_bit_vector loop
               if Bool then
                  secondNumB := secondNumB + (2 ** (uh - 1));
               end if;
               uh := uh + 1;
            end loop;

            IO.Put_Line ("Part B sum is" & Natural'Image (secondNumB * firstNumB));
         end;
      end;

   end runAoc;

end Aoc.P2021_3;
