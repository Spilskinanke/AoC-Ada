with Aoc.P2022_1; with Aoc.P2022_2; with Aoc.P2022_3; with Aoc.P2022_4; with Aoc.P2022_5;
with Aoc.P2021_1; with Aoc.P2021_2; with Aoc.P2021_3; with Aoc.P2021_4;
with Aoc.P2020_1; with Aoc.P2020_2;

package body Aoc is

   procedure AocNop (input : Filename_Ptr) is
      package IO renames Ada.Text_IO;
   begin
      IO.Put_Line ("No Solution for " & input.all);
   end AocNop;

   function buildMap return SolutionMap is

      noop_ptr : constant Solution := AocNop'Access;
      arr      : SolutionMap := [others => [others => noop_ptr]];

   begin

      arr (2022, 1) := Aoc.P2022_1.ptr; arr (2021, 1) := Aoc.P2021_1.ptr; arr (2020, 1) := Aoc.P2020_1.ptr;
      arr (2022, 2) := Aoc.P2022_2.ptr; arr (2021, 2) := Aoc.P2021_2.ptr; arr (2020, 2) := Aoc.P2020_2.ptr;
      arr (2022, 3) := Aoc.P2022_3.ptr; arr (2021, 3) := Aoc.P2021_3.ptr;
      arr (2022, 4) := Aoc.P2022_4.ptr; arr (2021, 4) := Aoc.P2021_4.ptr;
      arr (2022, 5) := Aoc.P2022_5.ptr;

      return arr;

   end buildMap;

   all_puzzles : aliased constant SolutionMap := buildMap;

   function acccessSolutionMap return access constant SolutionMap is begin
      return all_puzzles'Access;
   end acccessSolutionMap;

   overriding
   function "=" (Left, Right : Puzzle) return Boolean is begin
      return Left.Y = Right.Y and then
         Left.D = Right.D and then
         Left.I = Right.I;
   end "=";

   function "<" (Left, Right : Puzzle) return Boolean is begin
      if Left.Y = Right.Y then
         return Left.D < Left.D;
      else
         return Left.Y < Right.Y;
      end if;
   end "<";

   function run (P : Puzzle) return runStatus is begin
      IO.Put_Line ("Starting AOC" & P.Y'Image & " Day" & P.D'Image & ".");
      IO.Put_Line ("With file " & P.I.all);
      acccessSolutionMap.all (P.Y, P.D) (P.I);
      IO.New_Line;
      return Success;
   end run;

end Aoc;
