with Ada.Text_IO;
procedure main is

   type NumC is record
      n : Integer;
   end record;

   x : NumC := (n => 10);

begin
   Ada.Text_IO.Put_Line(Integer'Image(x.n));
end main;
