with Ada.Containers.Vectors; with Ada.Strings.Text_Buffers;

generic
   type E is private;
package Utils.Stacks is

   type Stack is tagged private;
   type Stack_Ptr is not null access Stack;

   type Element_List is array (Positive range <>) of E;

   function pop (S : in out Stack) return E;
   function pop (S : in out Stack; Amount : Positive) return Element_List;

   function look (S : Stack) return E;

   procedure push (S : in out Stack; guh : E);
   procedure push (S : in out Stack; Items : Element_List; byElement : Boolean);

private

   package Vectors is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                  Element_Type => E);
   type Stack is tagged record
      vec : Vectors.Vector := Vectors.Empty_Vector;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; S_tack : Stack);

end Utils.Stacks;
