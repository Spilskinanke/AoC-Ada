with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Aoc.P2021_2 is

   procedure runAoc (input : Filename_Ptr) is

      package STR renames Ada.Strings.Fixed;

      x_pos, y_pos : Integer := 0;
      x_pos2, y_pos2, aim : Integer := 0;

      type IntegerOp is access procedure (x : Integer);

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
      goUpAccess      : constant IntegerOp := goUp'Access;
      goDownAccess    : constant IntegerOp := goDown'Access;

      type Direction is (Forward, Upward, Downward, None);
      function getDirection (char : Character) return Direction is begin
         return (case char is
                    when 'f'    => Forward,
                    when 'u'    => Upward,
                    when 'd'    => Downward,
                    when others => None);

      end getDirection;
      func_lookup : constant array (Direction) of IntegerOp := [goForwardAccess,
                                                                goUpAccess,
                                                                goDownAccess,
                                                                null];

      file_name : constant String := input.all;
      file : IO.File_Type;

   begin

      IO.Open (file, IO.In_File, file_name);

      while not IO.End_Of_File (file) loop

         declare
            line   : constant String    := IO.Get_Line (file);
            direct : constant Direction := getDirection (line (line'First));
            value  : constant Integer   :=
               Integer'Value (line (STR.Index (line, " ") .. line'Last));
         begin
            func_lookup (direct)(value);
         end;

      end loop;

      declare
         total_A : constant Integer := x_pos * y_pos;
         total_B : constant Integer := x_pos2 * y_pos2;
      begin
         IO.Put_Line ("The total for Part A is" & total_A'Image);
         IO.Put_Line ("The total for Part B is" & total_B'Image);
      end;

      IO.Close (file);

   end runAoc;
end Aoc.P2021_2;
