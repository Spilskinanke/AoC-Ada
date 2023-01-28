with Ada.Text_IO;
with Ada.Strings.Fixed;

with Ada.Containers.Hashed_Maps;

package body Aoc2021_2 is

   procedure runAoc is

      package IO renames Ada.Text_IO;
      package STR renames Ada.Strings.Fixed;

      x_pos, y_pos : Integer := 0;
      x_pos2, y_pos2, aim : Integer := 0;

      type IntegerOp is access procedure (x : Integer);
      subtype Direction is Character with Static_Predicate =>
         Direction in 'f' | 'u' | 'd';

      procedure goForward (x : Integer) is begin
         x_pos := x_pos + x;

         x_pos2 := x_pos2 + x;
         y_pos2 := y_pos2 + (x * aim);
      end goForward;

      procedure goUp (x : Integer) is begin
         y_pos := y_pos - x;

         aim := aim - x;
      end goUp;

      procedure goDown (x : Integer) is begin
         y_pos := y_pos + x;

         aim := aim + x;
      end goDown;

      goForwardAccess : constant IntegerOp := goForward'Access;
      goUpAccess : constant IntegerOp := goUp'Access;
      goDownAccess : constant IntegerOp := goDown'Access;

      function calcHash (val : Direction) return Ada.Containers.Hash_Type is
      begin
         return Ada.Containers.Hash_Type (Direction'Pos (val));
      end calcHash;

      package FunctionMap is new Ada.Containers.Hashed_Maps
         (Key_Type        => Direction,
          Element_Type    => IntegerOp,
          Hash            => calcHash,
          Equivalent_Keys => "=");

      function buildMap (forward, up, down : IntegerOp) return FunctionMap.Map is
         temp_map : FunctionMap.Map;
      begin

         FunctionMap.Include (temp_map, 'f', forward);
         FunctionMap.Include (temp_map, 'u', up);
         FunctionMap.Include (temp_map, 'd', down);

         return temp_map;
      end buildMap;

      function_map : constant FunctionMap.Map := buildMap (goForwardAccess,
                                                             goUpAccess,
                                                             goDownAccess);

      file_name : constant String := "share/aoc2021_02.txt";
      file : IO.File_Type;

   begin

      IO.New_Line;
      IO.Put_Line ("Starting AOC2021 Day 2.");

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         declare
            line   : constant String    := IO.Get_Line (file);
            direct : constant Direction := Direction (line (line'First));
            value  : constant Integer   :=
               Integer'Value (line (STR.Index (line, " ") .. line'Last));
         begin
            function_map.Element (direct)(value);
         end;

      end loop;

      declare
         total_A : constant Integer := x_pos * y_pos;
         total_B : constant Integer := x_pos2 * y_pos2;
      begin
         IO.Put_Line ("The total for Part A is" & total_A'Image);
         IO.Put_Line ("The total for Part B is" & total_B'Image);
      end;

   end runAoc;
end Aoc2021_2;
