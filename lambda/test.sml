
val g = FunExpr("g",VarExpr("g")); (* identity *)

eval(FunExpr("y",VarExpr("y")));
eval(FunExpr("y",VarExpr("x")));
eval(ApplyExpr(FunExpr("y",VarExpr("y")),g));
eval(ApplyExpr(FunExpr("y",FunExpr("x",VarExpr("x"))),g));
eval(ApplyExpr(FunExpr("y",FunExpr("x",VarExpr("y"))),g));

