with Aoc.Files;
with Aoc;
with Ada.Text_IO;
procedure Aoc_Practice is
   use Aoc.Files;
   use Aoc;
   temp_puzzle_search : FullStatus;
begin
   temp_puzzle_search.status := hasYearDay;
   for Puzz of SearchForInput (".", temp_puzzle_search) loop
      if run (Puzz) = Failed then
         Ada.Text_IO.Put_Line ("FAILED!");
      end if;
   end loop;

end Aoc_Practice;
