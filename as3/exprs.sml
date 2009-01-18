(* type check an AST for the following MinML program:
   (fun f(x:bool):bool is f(x) end) 
   ( (fun g(y:int->int):bool is y(0)=0 end) 
     (fun h(z:int):int is z*z*z end) )
*)
let
  val fBody = ApplyExpr(VarExpr("f"),VarExpr("x"))
  val gBody = OpExpr(ApplyExpr(VarExpr("y"),NumExpr(0)),Equal,NumExpr(0))
  val hBody = OpExpr(OpExpr(VarExpr("z"),Times,VarExpr("z")),Times,VarExpr("z"))
in
  tc(ApplyExpr(FunExpr("f","x",Bool,Bool,fBody),
               ApplyExpr(FunExpr("g","y",Arrow(Int,Int),Bool,gBody),
                         FunExpr("h","z",Int,Int,hBody))))
end;

(* type check the same AST, but incorrectly change g from an
   (int->int)->bool function to an (int->bool)->bool function *)
let
  val fBody = ApplyExpr(VarExpr("f"),VarExpr("x"))
  val gBody = OpExpr(ApplyExpr(VarExpr("y"),NumExpr(0)),Equal,NumExpr(0))
  val hBody = OpExpr(OpExpr(VarExpr("z"),Times,VarExpr("z")),Times,VarExpr("z"))
in
  tc(ApplyExpr(FunExpr("f","x",Bool,Bool,fBody),
               ApplyExpr(FunExpr("g","y",Arrow(Int,Bool),Bool,gBody),
                         FunExpr("h","z",Int,Int,hBody))))
end;

(* incorrectly use an undeclared variable *)
tc(OpExpr(NumExpr(5),Plus,VarExpr("x")));

(* type check an AST for the following MinML program:
   if (fun f(x:bool):int is f(x) end)(true) then true else false
*)
tc(IfExpr(ApplyExpr(
            FunExpr("f","x",Bool,Int,ApplyExpr(VarExpr("f"),VarExpr("x"))),
            TrueExpr),
         TrueExpr,
         FalseExpr));

(* type check an AST for the following MinML program:
   if (fun f(x:bool):bool is f(x) end)(true) then true else false
*)
tc(IfExpr(ApplyExpr(
            FunExpr("f","x",Bool,Bool,ApplyExpr(VarExpr("f"),VarExpr("x"))),
            TrueExpr),
          TrueExpr,
          FalseExpr));

(* type check an AST for the following MinML program:
   fun f(x:int):int->int->int is 
     fun g(y:int):int->int is 
       fun h(z:int):int is x+y+z end 
     end 
   end
*)
tc(FunExpr("f","x",Int,Arrow(Int,Arrow(Int,Int)),
     FunExpr("g","y",Int,Arrow(Int,Int),
       FunExpr("h","z",Int,Int,
         OpExpr(OpExpr(VarExpr("x"),Plus,VarExpr("y")),Plus,VarExpr("z"))))));

(* type check the same AST, but incorrectly declare f to return an int->int *)
tc(FunExpr("f","x",Int,Arrow(Int,Int),
     FunExpr("g","y",Int,Arrow(Int,Int),
       FunExpr("h","z",Int,Int,
         OpExpr(OpExpr(VarExpr("x"),Plus,VarExpr("y")),Plus,VarExpr("z"))))));

