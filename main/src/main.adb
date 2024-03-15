with Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
use Ada.Containers.Vectors;

procedure Main is
   type ExprC_Type is (NumC_Type, IfC_Type, AppC_Type, IdC_Type, StrC_Type, LamC_Type);
   type Value_Type is (NumV_Type, BoolV_Type, StrV_Type, CloV_Type, PrimV_Type);

   type NumC;
   type IfC;
   type AppC;
   type StrC;
   type LamC;
   type IdC;

   type NumV;
   type StrV;
   type BoolV;
   type PrimV;
   type CloV;
   
   type ExprC (Exp : ExprC_Type) is record
      case Exp is
         when NumC_Type =>
            Num : NumC;
         when IfC_Type =>
            Iff : IfC;
         when AppC_Type =>
            App : AppC;
         when IdC_Type =>
            Id : IdC;
         when StrC_Type =>
            Str : StrC;
         when LamC_Type =>
            Lam : LamC;
      end case;
   end record;

    type Value (Val : Value_Type) is record
      case Val is
         when NumV_Type =>
            Num : NumV;
         when StrV_Type =>
            Str : StrV;
         when BoolV_Type =>
            Bool : BoolV;
         when CloV_Type =>
            Clo : CloV;
         when PrimV_Type =>
            Prim : PrimV;
      end case;
   end record;

   type ExprC_Arr is Array(Positive range <>) of ExprC;  
   type Str_Arr is Array(Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;
   type Value_Arr is Array(Positive range <>) of Value;

   type NumC is record
      n : Integer;
   end record;

   type IfC is record
      test : ExprC;
      if_true : ExprC;
      if_false : ExprC;
   end record;

   type AppC is record
      func : ExprC;
      arg : ExprC_Arr;
   end record;

   type IdC is record
      func : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type StrC is record
      s : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type LamC is record
      arg : Str_Arr;
      bod : ExprC;
   end record;


   type NumV is record
      n : Integer;
   end record;

   type BoolV is record
      b : Boolean;
   end record;

   type StrV is record
      s : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type CloV is record
      arg : Ada.Strings.Unbounded.Unbounded_String;
      bod : ExprC;
   end record;

   type PrimV is record
      sym : Ada.Strings.Unbounded.Unbounded_String;
   end record;


   type Binding is record
      id : Ada.Strings.Unbounded.Unbounded_String;
      val : Value;
   end record;

   type Env_Type is Array(Positive range <>) of Binding;

   
   Prim_Plus : constant PrimV := (Sym => To_Unbounded_String("+"));
   Prim_Minus : constant PrimV := (Sym => To_Unbounded_String("-"));
   Prim_Multiply : constant PrimV := (Sym => To_Unbounded_String("*"));
   Prim_Divide : constant PrimV := (Sym => To_Unbounded_String("/"));
   Prim_LessThanOrEqual : constant PrimV := (Sym => To_Unbounded_String("<="));
   Prim_Equal : constant PrimV := (Sym => To_Unbounded_String("equal?"));
   Prim_True : constant PrimV := (Sym => To_Unbounded_String("equal?"));
   Prim_False : constant PrimV := (Sym => To_Unbounded_String("equal?"));


   Top_Env : constant Env_Type := (
      1 => (Id => To_Unbounded_String("+"), Val => (PrimV => Prim_Plus)),
      2 => (Id => To_Unbounded_String("-"), Val => (PrimV => Prim_Minus)),
      3 => (Id => To_Unbounded_String("*"), Val => (PrimV => Prim_Multiply)),
      4 => (Id => To_Unbounded_String("/"), Val => (PrimV => Prim_Divide)),
      5 => (Id => To_Unbounded_String("<="), Val => (PrimV => Prim_LessThanOrEqual)),
      6 => (Id => To_Unbounded_String("equal?"), Val => (PrimV => Prim_Equal)),
      7 => (Id => To_Unbounded_String("true"), Val => (PrimV => Prim_Equal)),
      8 => (Id => To_Unbounded_String("false"), Val => (PrimV => Prim_Equal))
      );

   function Extend_Env(New_Env : Env_Type; Old_Env : Env_Type) return Env_Type is
   begin
      if New_Env'Length = 0 then
         return Old_Env;
      else
         return Old_Env & New_Env(New_Env'First .. New_Env'Last);
      end if;
   end Extend_Env;

   function Bind_Helper(Ids : Str_Arr; Ns : Value_Arr; Index : Natural) return Env_Type is
   begin
      if Index > Ids'Length then
         return (1 .. 0 => (Id => To_Unbounded_String(""), Val => (Sym => To_Unbounded_String(""))));
      else
         return (Index => (Id => Ids(Index), Val => Ns(Index))) & Bind_Helper(Ids, Ns, Index + 1);
      end if;
   end Bind_Helper;

   function Bind(Ids : Str_Arr; Ns : Value_Arr) return Env_Type is
   begin
      return Bind_Helper(Ids, Ns, 1);
   end Bind;

   function Lookup(Id : Ada.Strings.Unbounded.Unbounded_String; Env : Env_Type) return Value is
   begin
      if Env'Length = 0 then
         raise Program_Error with "OAZO: unbound identifier: " & Ada.Strings.Unbounded.To_String(Id);
      elsif Id = Env(Env'First).Id then
         return Env(Env'First).Val;
      else
         return Lookup(Id, Env(Env'First + 1 .. Env'Last));
      end if;
   end Lookup;

   function Interp(Exp : ExprC; Env : Env_Type) return Value is
   begin
      case Exp.Exp is
         when NumC_Type =>
            return (Val => NumV_Type, Num => (n => Exp.Num.n));
         when StrC_Type =>
            return (Val => StrV_Type, Str => (s => Exp.Str.s));
         when IfC_Type =>
            declare
               Bool : Value;
            begin
               Bool := Interp(Exp.Iff.test, Env);
               if not Bool.Bool.b then
                  raise Program_Error with "OAZO: invalid if test value: " & Ada.Strings.Unbounded.To_String(Exp);
               elsif Bool.Bool.b then
                  return Interp(Exp.Iff.if_true, Env);
               else
                  return Interp(Exp.Iff.if_false, Env);
               end if;
            end;
         when LamC_Type =>
            return (Val => (Clo => (Arg => Exp.Lam.arg, Bod => Exp.Lam.bod, Env => Env)));
         when AppC_Type =>
            declare
               Func : Value;
               Interp_Args : array(Positive range <>) of Value;
            begin
               Func := Interp(Exp.App.func, Env);
               for I in Interp_Args'Range loop
                  Interp_Args(I) := Interp(Exp.App.arg(I), Env);
               end loop;

               case Func.Val is
                  when PrimV_Type =>
                     if Exp.App.arg'Length = 2 then
                        return Prim_Eval(Func.Prim.sym, Interp_Args);
                     else
                        raise Program_Error with "OAZO: wrong arity: " & Ada.Strings.Unbounded.To_String(Exp);
                     end if;
                  when CloV_Type =>
                     if Exp.App.arg'Length = Exp.App.arg'Length then
                        return Interp(Func.Clo.bod, Extend_Env(Bind(Exp.App.arg, Interp_Args), Func.Clo.env));
                     else
                        raise Program_Error with "OAZO: wrong arity: " & Ada.Strings.Unbounded.To_String(Exp);
                     end if;
                  when others =>
                     raise Program_Error with "OAZO: application of a non-closure: " & Ada.Strings.Unbounded.To_String(Exp);
               end case;
            end;
         when IdC_Type =>
            return Lookup(Exp.Id.func, Env);
         when others =>
            raise Program_Error with "OAZO: invalid expression type: " & Ada.Strings.Unbounded.To_String(Exp);
      end case;
   end Interp;

   procedure Serialize (val : Value) is
   begin
      case val.Val is
         when NumV_Type =>
            Ada.Text_IO.Put_Line(Integer'Image(val.Num.n));
         when StrV_Type =>
            Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(val.Str.s));
         when BoolV_Type =>
            Ada.Text_IO.Put_Line(Boolean'Image(val.Bool.b));
         when CloV_Type =>
            Ada.Text_IO.Put_Line("#<procedure>");
         when PrimV_Type =>
            Ada.Text_IO.Put_Line("#<primop>");
      end case;
   end Serialize; 

   NumExpr : NumC := (n => 10);
   StrExpr : StrC := (s => To_Unbounded_String("string"));
   -- StrExpr : ExprC := (Exp => StrC_Type, Str => (s => Ada.Strings.Unbounded.To_Unbounded_String("string")));
   -- LamExpr : ExprC := (Exp => LamC_Type, Lam => (arg => Ada.Strings.Unbounded.To_Unbounded_String("lam arg"), bod => (NumC := (n => 42))));
   -- AppExpr : ExprC := (Exp => AppC_Type, App => (func => Ada.Strings.Unbounded.To_Unbounded_String("app"), arg => 3));
   -- IdExpr : ExprC := (Exp => IdC_Type, Id => (func => Ada.Strings.Unbounded.To_Unbounded_String("id")));

   begin
      Serialize(Interp(NumExpr, Top_Env));
      Serialize(Interp(NumExpr, Top_Env), "Test 1 failed: NumC");
      Serialize(Interp(StrExpr, Top_Env), "Test 1 failed: StrC");
end Main;
