package body Utils.Optional is

   function isEmpty (self : Optional) return Boolean is begin
      return self.opt = Empty;
   end isEmpty;

   function get (self : Optional) return E is begin
      if isEmpty (self) then
         raise EMPTY_ACCESS_EXCEPTION;
      else
         return self.item;
      end if;
   end get;

   procedure set (self : in out Optional; item : E) is
      new_item : Optional (Present);
   begin
      new_item.item := item;
      self := new_item;
   end set;

   procedure clear (self : in out Optional) is
      no_item : Optional (Empty);
   begin
      self := no_item;
   end clear;

   function wrap (item : E) return Optional is
      new_item : Optional (Present);
   begin
      new_item.item := item;
      return new_item;
   end wrap;

end Utils.Optional;
