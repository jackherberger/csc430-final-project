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

	function Interp (exp : ExprC_Type) return Integer is
   begin
      case exp is
         when NumC_Type =>
            return 1;
         when AppC_Type =>
            return 1;
         when IdC_Type =>
            return 2;
         when StrC_Type =>
            return 3;
         when LamC_Type =>
            return 4;
         when others =>
            return -1;
      end case;
   end Interp;

   X : NumC := (N => 10);
   Expr : ExprC_Type := NumC_Type;

begin
   Ada.Text_IO.Put_Line(Integer'Image(Interp(Expr)));
end Main;