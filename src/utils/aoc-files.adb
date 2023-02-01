with GNAT.Regpat;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Text_IO;

package body Aoc.Files is

   package FS renames Ada.Directories;
   package RGX renames GNAT.Regpat;
   package IO renames Ada.Text_IO;

   year_pattern : constant RGX.Pattern_Matcher := RGX.Compile ("(\d{4})");
   day_pattern  : constant RGX.Pattern_Matcher := RGX.Compile ("(\d{1,2})");

   guh : constant FS.Filter_Type := [True, True, False];

   function isValid (I : String; Y : out Year; D : out Day)
                     return FilenameStatus is

      package STR renames Ada.Strings.Fixed;
      use GNAT.Regpat;

      input : String := I;
      rgx_match : Match_Array (1 .. 1);
      status : FilenameStatus := Invalid;

   begin

      RGX.Match (year_pattern, input, rgx_match);

      if rgx_match (1) /= No_Match then
         begin
            Y := Year'Value (input (rgx_match (1).First .. rgx_match (1).Last));
            if Y /= 2014 then
               status := hasYear;
            end if;
         exception
            when Constraint_Error => null;
         end;
         STR.Delete (input, rgx_match (1).First, rgx_match (1).Last);
      end if;

      RGX.Match (day_pattern, input, rgx_match);
      if rgx_match (1) /= No_Match then
         begin
            D := Day'Value (input (rgx_match (1).First .. rgx_match (1).Last));
            if D /= 0 then
               status := FilenameStatus'Succ (status);
            end if;
         exception
            when Constraint_Error => null;
         end;
      end if;

      return status;

   end isValid;
   
   -----------------------------------------------------------------------------

   function SearchForInput (start_dir : String; dependencies : FullStatus)
                             return RunList.Vector is

      use Ada.Directories;
      Search : Search_Type;
      Dir    : Directory_Entry_Type;
      all_runs : RunList.Vector := RunList.Empty_Vector;

   begin

      Start_Search (Search,
                       Directory => start_dir,
                       Pattern   => "",
                       Filter    => guh);

      while More_Entries (Search) loop

         Get_Next_Entry (Search, Dir);
         declare
            path_name : constant String := Full_Name (Dir);
            file_name : constant String := Simple_Name (Dir);
         begin
            if file_name in "." | ".." or else
               Extension (file_name) not in "" | "txt" | "csv" then
               null;
            elsif Kind (Dir) /= Directory then
               declare
                  Y         : Year;
                  D         : Day;
                  regex_result : constant FilenameStatus := isValid (file_name, Y, D);
               begin
                  if regex_result = hasYearDay or else
                     regex_result = dependencies.status or else
                     dependencies.status = Invalid then
                     declare
                        fptr : constant Filename_Ptr := new String'(path_name);
                        P    :  constant Puzzle :=
                                 (case dependencies.status is
                                     when hasYearDay => (Y, D, fptr),
                                     when hasYear    => (Y, dependencies.D, fptr),
                                     when hasDay     => (dependencies.Y, D, fptr),
                                     when Invalid    => (dependencies.Y, dependencies.D, fptr));
                     begin
                        all_runs.Append (P);
                     end;
                  end if;
               end;
            else
               all_runs.Append (SearchForInput (Full_Name (Dir), dependencies));
            end if;
         end;

      end loop;

      return all_runs;
   end SearchForInput;

end Aoc.Files;
