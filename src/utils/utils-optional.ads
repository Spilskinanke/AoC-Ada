generic
   type E is private;

package Utils.Optional is

   type Option_Type is (Present, Empty);
   type Optional (opt : Option_Type) is tagged private;

   function get (self : Optional) return E;
   procedure set (self : in out Optional; item : E);
   procedure clear (self : in out Optional);
   function wrap (item : E) return Optional;
   function isEmpty (self : Optional) return Boolean;

private

   type Optional (opt : Option_Type) is tagged record
      case opt is
         when Present => item : aliased E;
         when Empty => null;
      end case;
   end record;

   EMPTY_ACCESS_EXCEPTION : exception;

end Utils.Optional;
