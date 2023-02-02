generic
   type Element_Type is private;
   type Index_Type is (<>);
   type Array_Type is array (Index_Type) of Element_Type;
package Utils.ReverseArray is

   procedure reverse_arr (Item : in out Array_Type); 

end Utils.ReverseArray;
