with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.IO_Exceptions;

package body Aoc.P2021_4 is

   procedure runAoc (input : Filename_Ptr) is

      file : IO.File_Type;
      file_name : constant String := input.all;

      type Position is record
         card_loc, x_pos, y_pos : Positive;
      end record;

      subtype BingoNum is Integer range 0 .. 100;
      package I_IO is new Ada.Text_IO.Integer_IO (Num => BingoNum);
      type BingoSpot is record
         num : BingoNum;
         marked : Boolean := False;
      end record;
      type BingoCard is array (Natural range <>, Natural range <>) of access BingoSpot;

      function initialize return Natural is
         card_size : Natural := 0;
      begin
         IO.Open (file, IO.In_File, file_name);
         IO.Skip_Line (file);
         IO.Skip_Line (file);

         while not IO.End_Of_Line (file) loop
            declare
               dummy_int : BingoNum;
            begin
               I_IO.Get (file, dummy_int);
               card_size := card_size + 1;
            end;
         end loop;
         IO.Close (file);
         return card_size;
      end initialize;

      card_size : constant Natural := initialize;

      package PositionList is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                          Element_Type => Position,
                                                          "="          => "=");

      function Equivalent_Elem (Left, Right : PositionList.Vector) return Boolean is
         pragma Unreferenced (Right, Left);
      begin
         return False;
      end Equivalent_Elem;

      package PositionHash is new Ada.Containers.Hashed_Maps (Key_Type        => BingoNum,
                                                              Element_Type    => PositionList.Vector,
                                                              Hash            => Ada.Containers.Hash_Type'Mod,
                                                              Equivalent_Keys => "=",
                                                             "=" => Equivalent_Elem);

      type ConBingoCard is new BingoCard (1 .. card_size, 1 ..  card_size);
      type BingoCardData is record
         card : ConBingoCard;
         sum  : Integer := 0;
         won  : Boolean := False;
      end record;
      package CardHolder is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => BingoCardData,
                                                        "="          => "=");
      card_list : CardHolder.Vector := CardHolder.Empty_Vector;
      position_map : PositionHash.Map := PositionHash.Empty_Map;
      card_counter : Positive := 1;
      card_total : Positive := 1;

      procedure initializeBingoData is
         card_index : Positive := Positive'First;
      begin
         IO.Open (file, IO.In_File, file_name);
         IO.Skip_Line (file);

         while not IO.End_Of_File (file) loop
            declare
               new_card : ConBingoCard;
               card_data : BingoCardData;
            begin

               card_data.card := new_card;

               for I in new_card'Range (1) loop
                  for J in new_card'Range (2) loop
                     declare
                        new_bing : BingoNum;
                        bing_pos : constant Position := (card_index, I, J);
                        bing_dat : BingoSpot;
                     begin
                        I_IO.Get (file, new_bing);
                        bing_dat.num := new_bing;
                        card_data.card (I, J) := new BingoSpot'(bing_dat);
                        card_data.sum := card_data.sum + Integer (new_bing);

                        if not position_map.Contains (new_bing) then
                           position_map.Include
                              (new_bing, PositionList.Empty_Vector);
                        end if;
                        position_map.Reference
                           (new_bing).Element.all.Append (bing_pos);
                     end;
                  end loop;
               end loop;

               card_list.Append (card_data);
               card_index := card_index + 1;
            end;

         end loop;
         card_counter := card_index - 1;
         card_total := card_index - 1;
         IO.Close (file);

      end initializeBingoData;

      function hasBingo (card : ConBingoCard; x, y : Natural) return Boolean is
         x_bingo, y_bingo : Boolean := True;
      begin
         for I in card'Range (1) loop
            x_bingo := x_bingo and then card (x, I).marked;
            y_bingo := y_bingo and then card (I, y).marked;
         end loop;
         return x_bingo or y_bingo;
      end hasBingo;

      function calcScore (card : ConBingoCard; call : BingoNum)
                          return Integer is
         acc : Integer := 0;
      begin
         for I in card'Range (1) loop
            for J in card'Range (2) loop
               if not card (I, J).marked then
                  acc := acc + Integer (card (I, J).num);
               end if;
            end loop;
         end loop;
         return acc * Integer (call);

      end calcScore;

   begin

      initializeBingoData;

      IO.Open (file, IO.In_File, file_name);
      Play_Bingo :
      while not IO.End_Of_Line (file) loop
         declare
            bingo_call : BingoNum;
            dummy_char : Character;
         begin
            I_IO.Get (file, bingo_call);
            IO.Get (file, dummy_char);

            for pos of position_map.Element (bingo_call) loop
               declare
                  card_data : constant access BingoCardData
                     := card_list.Reference (pos.card_loc).Element;
               begin
                  card_data.card (pos.x_pos, pos.y_pos).marked := True;
                  card_data.sum := card_data.sum - bingo_call;

                  if not card_data.won and then
                     hasBingo (card_data.card, pos.x_pos, pos.y_pos) then

                     card_data.won := True;
                     if card_counter = card_total then
                        IO.Put_Line ("The sum for Part A is" &
                                        Integer'Image (card_data.sum * bingo_call));
                     elsif card_counter = 1 then
                        IO.Put_Line ("The sum for Part B is" &
                                        Integer'Image (card_data.sum * bingo_call));
                        exit Play_Bingo;
                     end if;
                     card_counter := card_counter - 1;
                  end if;
               end;
            end loop;
         exception
            when Ada.IO_Exceptions.End_Error =>
               IO.Put_Line ("Nobody wins.");
               exit Play_Bingo;
         end;
      end loop Play_Bingo;

      IO.Close (file);
   end runAoc;

end Aoc.P2021_4;
