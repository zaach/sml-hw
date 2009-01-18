(* This program type checks and evaluates the following MinML expression e:
     (fun fact(x: int ref):int is 
        if (!x)<2 then 1 
        else (!x) * fact( (fun f(y:int):int ref is x) (x:=(!x)-1) ))
     (ref 6)

   In ML, we would evaluate e with the following code:
     fun fact (x) = 
        if (!x)<2 then 1
        else (!x) * fact ( (fn y=>x) (x:=(!x)-1) );
     fact (ref 6);

   or more compactly:
     fun fact x = if !x<2 then 1 else !x * fact( x:=(!x)-1; x );
     fact (ref 6);
*)

let 
  val ifExpr = OpExpr(DerefExpr(VarExpr("x")),Less,NumExpr(2));
  val nestedFun = FunExpr("f","y",Int,Ref(Int),VarExpr("x"));
  val nestedArg = UpdateExpr(VarExpr("x"),
                             OpExpr(DerefExpr(VarExpr("x")),Minus,NumExpr(1)));
  val recursiveArg = ApplyExpr(nestedFun,nestedArg);
  val elseExpr = OpExpr(DerefExpr(VarExpr("x")),Times,
                        ApplyExpr(VarExpr("fact"),recursiveArg));
  val factBody = IfExpr(ifExpr,NumExpr(1),elseExpr);
  val e = ApplyExpr(FunExpr("fact","x",Ref(Int),Int,factBody),
                    NewRefExpr(NumExpr(6)));
in
  case tc(e)
  of SOME(_)=>
    (case eval(e)
     of NumExpr(n)=> print(Int.toString(n)^"\n")
      | _ => print("Error evaluating expression e\n"))
   | NONE=> print("Error: e is an ill-typed expression\n")
end;

