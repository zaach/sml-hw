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
  tc(e)
end;

eval(OpExpr(NumExpr(8),Plus,NumExpr(5)));
eval(OpExpr(NumExpr(8),Minus,NumExpr(5)));
eval(OpExpr(NumExpr(8),Less,NumExpr(5)));
eval(OpExpr(NumExpr(8),Equal,OpExpr(NumExpr(3),Plus,NumExpr(5))));

eval(OpExpr(TrueExpr,Equal,TrueExpr));
eval(OpExpr(FalseExpr,Equal,FalseExpr));
eval(OpExpr(TrueExpr,Equal,FalseExpr));
eval(OpExpr(FalseExpr,Equal,TrueExpr));

eval(IfExpr(TrueExpr,NumExpr(8),NumExpr(4)));

eval(NewRefExpr(NumExpr(6)));

eval(DerefExpr(NewRefExpr(NumExpr(6))));

eval(UpdateExpr(NewRefExpr(NumExpr(6)),NumExpr(7)));

eval(FunExpr("f","y",Int,Ref(Int),VarExpr("y")));

eval(ApplyExpr(FunExpr("f","y",Int,Int,VarExpr("y")),NumExpr(7)));

eval(ApplyExpr(FunExpr("f","y",Int,Int,OpExpr(VarExpr("y"),Plus,VarExpr("y"))),NumExpr(7)));

let
  val elseExpr = ApplyExpr( VarExpr("fac"), OpExpr(VarExpr("y"),Minus,NumExpr(1)) )

  val fbody = IfExpr(OpExpr(VarExpr("y"),Less,NumExpr(5)),NumExpr(1),elseExpr)
in
  eval(ApplyExpr(FunExpr("fac","y",Int,Int,fbody),NumExpr(7)))
end;

val factBody = IfExpr(OpExpr(VarExpr("x"),Less,NumExpr(2)),NumExpr(1),
                        OpExpr(VarExpr("x"),Times,ApplyExpr(VarExpr("fact"),
                                  OpExpr(VarExpr("x"),Minus,NumExpr(1)))));

val e = ApplyExpr(FunExpr("fact","x",Int,Int,factBody),NumExpr(6));

eval(e);

let
(*
fun fac(x) = (fun f(y) = x)(1);

fac(7);

*)
  val nestedFun = FunExpr("f","y",Int,Int,VarExpr("y"))
  val nestedArg = OpExpr(VarExpr("x"),Minus,NumExpr(1))
  val elseExpr = ApplyExpr(nestedFun,NumExpr(1))
                             
  val fbody = IfExpr(OpExpr(VarExpr("x"),Less,NumExpr(5)),NumExpr(1),elseExpr)
  val e = ApplyExpr(FunExpr("fac","x",Int,Int,elseExpr),NumExpr(7))
in
  eval(e)
end;

let

  val nestedFun = FunExpr("f","y",Int,Ref(Int),VarExpr("x"))
  val nestedArg = UpdateExpr(VarExpr("x"),
                             OpExpr(DerefExpr(VarExpr("x")),Minus,NumExpr(1)))
  val elseExpr = ApplyExpr(nestedFun,NumExpr(1))
                             
  val fbody = IfExpr(OpExpr(VarExpr("x"),Less,NumExpr(5)),NumExpr(1),elseExpr)
in
  eval(ApplyExpr(FunExpr("fac","x",Int,Ref(Int),elseExpr),NewRefExpr(NumExpr(7))))
end;

let 
  val ifExpr = OpExpr(DerefExpr(VarExpr("x")),Less,NumExpr(2));
  val nestedFun = FunExpr("f","y",Int,Ref(Int),VarExpr("x"));
  val nestedArg = UpdateExpr(VarExpr("x"),
                             OpExpr(DerefExpr(VarExpr("x")),Minus,NumExpr(1)));
  val recursiveArg = ApplyExpr(nestedFun,nestedArg);
  val elseExpr = OpExpr(DerefExpr(VarExpr("x")),Times,
                        ApplyExpr(VarExpr("fact"),recursiveArg));
  val elseExpr2 = OpExpr(DerefExpr(VarExpr("x")),Times,DerefExpr(VarExpr("x")));
  val factBody = IfExpr(ifExpr,NumExpr(1),elseExpr);
  val e = ApplyExpr(FunExpr("fact","x",Ref(Int),Int,factBody),
                    NewRefExpr(NumExpr(6)));
  val e2 = ApplyExpr(FunExpr("fact","x",Ref(Int),Int,IfExpr(ifExpr,NumExpr(1),nestedArg)),
                    NewRefExpr(NumExpr(6)));
in
  eval(e2)
end;

(*
*)

