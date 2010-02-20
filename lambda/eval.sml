(* 
    Evaluates lambda calculus expressions (ASTs)
    using call-by-value stategy

    - Zachary Carter
*)

exception cannotStep;

fun eval(expr) = 
  let    
    (* substitution *)
    fun sub (e as VarExpr(s)) x v = if s = x then v else e
    |   sub (FunExpr(y,body)) x v =
          if x = y
            then FunExpr(y,body)
            else FunExpr(y,(sub body x v))
    |   sub (ApplyExpr(e1,e2)) x v = ApplyExpr((sub e1 x v), (sub e2 x v))
    
    (* dynamic semantics *)
    fun step (e as FunExpr(_,_)) = e
    |   step (ApplyExpr(v1 as FunExpr(param,body),e2)) =
      (fn (v2) => step (sub body param v2) (* call *)
        ) (step e2) (* e2 -> v2 *)
    |   step (ApplyExpr(e1,e2)) =
      (fn (v1) => step (ApplyExpr(v1,e2))
        ) (step e1) (* e1 -> v1 *)
    |   step _ = raise cannotStep
    
  in
    (* return final expression *)
    step expr
  end;
  
