with Ada.Strings.Text_Buffers;
with System.Put_Images;

package body Utils.Stacks is

   function pop (S : in out Stack) return E is
      item : constant E := S.vec.Last_Element;
   begin
      S.vec.Delete_Last;
      return item;
   end pop;

   function look (S : Stack) return E is
   begin
      return S.vec.Last_Element;
   end look;

   function pop (S : in out Stack; Amount : Positive) return Element_List is

      items : Element_List (Positive'First .. Amount);
   begin

      for I in items'Range loop
         items (I) := pop (S);
      end loop;

      return items;
   end pop;

   procedure push (S : in out Stack; guh : E) is begin
      Vectors.Append (S.vec, guh);
   end push;

   procedure push (S : in out Stack; Items : Element_List; byElement : Boolean) is begin
      if byElement then
         for I in Items'Range loop
            push (S, Items (I));
         end loop;
      else
         for I in reverse Items'Range loop
            push (S, Items (I));
         end loop;
      end if;
   end push;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; S_tack : Stack)
   is
      First_Time : Boolean := True;
      use System.Put_Images;
   begin
      Array_Before (S);

      for X of S_tack.vec loop
         if First_Time then
            First_Time := False;
         else
            Simple_Array_Between (S);
         end if;

         E'Put_Image (S, X);
      end loop;

      Array_After (S);
   end Put_Image;

end Utils.Stacks;
