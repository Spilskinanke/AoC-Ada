
package Aoc.Files is

   type FilenameStatus is (Invalid, hasDay, hasYear, hasYearDay);
   type FullStatus is record
      status : FilenameStatus := Invalid;
      D      : Day := 0;
      Y      : Year := 2014;
   end record;

   function isValid (I : String; Y : out Year; D : out Day) return FilenameStatus;

   function SearchForInput (start_dir : String; dependencies : FullStatus) return RunList.Vector;

end Aoc.Files;
