with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Utils.Stacks;

package body Aoc.P2022_5 is

   procedure runAoc(input : Filename_Ptr) is

      package UB renames Ada.Strings.Unbounded;
      package INT_IO is new Ada.Text_IO.Integer_IO (Num => Positive);
      package Stacks is new Utils.Stacks (E => Character);

      type StackList is array (Positive range <>) of Stacks.Stack_Ptr;
      type StringList is array (Positive range <>) of UB.Unbounded_String;

      function getStacks (line_length : Integer) return Integer is begin
         return (line_length + 1) / 4;
      end getStacks;

      procedure putStringList (list : in out StringList; line : String) is
         index : Positive := 2;

         function filePosToListPos (num : Positive) return Positive is begin

            return (num - 1) - (3 * (num - 2) / 4);

         end filePosToListPos;

      begin
         while index <= line'Last - 1 loop
            if line (index) /= ' ' then
               UB.Append (list (filePosToListPos (index)), line (index));
            end if;
            index := index + 4;
         end loop;

      end putStringList;

      procedure buildStack (S : Stacks.Stack_Ptr; line : UB.Unbounded_String) is
         bounded_line : constant String := UB.To_String (line);
      begin

         for I in reverse bounded_line'Range loop
            S.all.push (bounded_line (I));
         end loop;
      end buildStack;

      file : IO.File_Type;
      file_name : constant String := input.all;

   begin

      IO.Open (File => file,
               Mode => IO.In_File,
               Name => file_name);

      declare
         first_line : constant String := IO.Get_Line (file);
         stack_lines : StringList (Positive'First ..
                                      Positive (getStacks (first_line'Length)));

         stack_listA, stack_listB : constant StackList (Positive'First ..
                                    Positive (getStacks (first_line'Length)))
            := (others => new Stacks.Stack);

      begin
         putStringList (stack_lines, first_line);

         loop
            declare
               line : constant String := IO.Get_Line (file);
            begin
               exit when line'Length = 0;

               if line (1 .. 2) /= " 1" then
                  putStringList (stack_lines, line);
               end if;

            end;

         end loop;

         for I in stack_lines'Range loop
            buildStack (S => stack_listA (I),
                        line => stack_lines (I));
            buildStack (S => stack_listB (I),
                        line => stack_lines (I));

         end loop;

         while not IO.End_Of_File (file) loop
            declare
               dummy_string_4 : String (1 .. 4);
               dummy_string_5 : String (1 .. 5);
               dummy_string_6 : String (1 .. 6);
               amount, source, dest : Positive;
            begin
               IO.Get (file, dummy_string_5);
               INT_IO.Get (file, amount);
               IO.Get (file, dummy_string_6);
               INT_IO.Get (file, source);
               IO.Get (file, dummy_string_4);
               INT_IO.Get (file, dest);

               stack_listA (dest).all.push (stack_listA (source).pop (amount), True);
               stack_listB (dest).all.push(stack_listB(source).all.pop(amount), False);
            end;
         end loop;

         IO.Put ("Word for Part A is : ");
         for S of stack_listA loop
            IO.Put ("" & S.all.look);

         end loop;

         IO.New_Line;

         IO.Put ("Word for Part B is : ");
         for S of stack_listB loop
            IO.Put ("" & S.all.look);

         end loop;
         IO.New_Line;
      end;

      IO.Close (file);

   end runAoc;

end Aoc.P2022_5;
