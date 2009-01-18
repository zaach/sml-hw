(* This program type checks and evaluates the following MinML expression e:
     (fun fact(x:int):int is if x<2 then 1 else x*fact(x-1))(6)
*)

let 
  val factBody = IfExpr(OpExpr(VarExpr("x"),Less,NumExpr(2)),NumExpr(1),
                        OpExpr(VarExpr("x"),Times,ApplyExpr(VarExpr("fact"),
                                  OpExpr(VarExpr("x"),Minus,NumExpr(1)))));
  val e = ApplyExpr(FunExpr("fact","x",Int,Int,factBody),NumExpr(6));
in
  case tc(e)
  of SOME(_)=>
    (case eval(e)
     of NumExpr(n)=> print(Int.toString(n)^"\n")
      | _ => print("Error evaluating expression e\n"))
   | NONE=> print("Error: e is an ill-typed expression\n")
end;

