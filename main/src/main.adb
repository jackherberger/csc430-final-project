with Ada.Text_IO;
procedure main is

   	type ExprC_Type is (NumC_Type, AppC_Type, IdC_Type, StrC_Type, LamC_Type);

	type NumC is record
   		n : Integer;
  	end record;

	type AppC is record
		func : ExprC_Type;
		arg : ExprC_Type;
	end record;

	type IdC is record
		func : String(1..10);
	end record;

	type StrC is record
		s : String(1..10);
	end record;

	type LamC is record
		arg : String(1..10);
	end record;

   	x : NumC := (n => 10);

begin
  	 Ada.Text_IO.Put_Line(Integer'Image(x.n));
end main;
