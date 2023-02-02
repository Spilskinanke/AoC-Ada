package body Utils.ReverseArray is

   procedure reverse_arr (Item : in out Array_Type) is 
      procedure Swap (A, B : in out Element_Type) is 
         Temp : constant Element_Type := A; 
      begin 
         A := B; 
         B := Temp; 
      end Swap; 
      High : Index_Type := Item'Last; 
      Low  : Index_Type := Item'First; 
   begin 
      while Low < High loop 
         Swap (Item (Low), Item (High)); 
         Low  := Index_Type'Succ (Low); 
         High := Index_Type'Pred (High); 
      end loop; 
   end reverse_arr; 

end Utils.ReverseArray;
