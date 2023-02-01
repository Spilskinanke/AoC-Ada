with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

package Aoc is

   package IO renames Ada.Text_IO;

   type Day is new Natural range 0 .. 25; -- 0 is invalid
   type Year is new Positive range 2014 .. 2023; -- 2014 is invalid
   type Filename_Ptr is access constant String;

   type Solution is access procedure (input : Filename_Ptr);
   type SolutionMap is array (Year, Day) of Solution;


   type Puzzle is record
      Y : Year := 2014;
      D : Day := 0;
      I : Filename_Ptr;
   end record;

   function "<" (Left, Right : Puzzle) return Boolean;

   overriding
   function "=" (Left, Right : Puzzle) return Boolean;

   package RunList is new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                                             Element_Type => Puzzle,
                                                             "="          =>  "=");

   function acccessSolutionMap return access constant SolutionMap;

   type runStatus is (Failed, Success);
   function run(P : Puzzle) return runstatus;

end Aoc;
