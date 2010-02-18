(* 
    Programming Languages - Fall 2008 - J Ligatti
    Assignment 4
    
    Eval MinML expressions

    I pledge my Honor that I have not cheated on this assignment.
    - Zachary Carter
*)

exception cannotStep;

fun eval(expr) = 
  let    
    val mem = nil
    val addr = 0
    
    (* substitution *)
    fun sub (e as NumExpr(_)) _ _ = e
    |   sub (e as LocExpr(_)) _ _ = e
    |   sub (e as TrueExpr) _ _ = e
    |   sub (e as FalseExpr) _ _ = e
    |   sub (e as VarExpr(s)) x v = if s = x then v else e
    |   sub (NewRefExpr(e1)) x v = NewRefExpr(sub e1 x v)
    |   sub (DerefExpr(e1)) x v = DerefExpr(sub e1 x v)
    |   sub (UpdateExpr(e1,e2)) x v = UpdateExpr((sub e1 x v),(sub e2 x v))
    |   sub (OpExpr(e1,oper,e2)) x v = OpExpr((sub e1 x v), oper, (sub e2 x v))
    |   sub (IfExpr(e1,e2,e3)) x v = IfExpr((sub e1 x v), (sub e2 x v), (sub e3 x v))
    |   sub (FunExpr(f,y,tp,tr,body)) x v =
          if x = f orelse x = y
            then FunExpr(f,y,tp,tr,body)
            else FunExpr(f,y,tp,tr,(sub body x v))
    |   sub (ApplyExpr(e1,e2)) x v = ApplyExpr((sub e1 x v), (sub e2 x v))
    
    (* memory lookup *)
    fun memory(L as LocExpr(loc), (l,e)::vals) = if loc = l then e else memory(L, vals)
    |   memory(_,_) = raise cannotStep
    
    (* dynamic semantics *)
    fun step mem nextAddr (e as NumExpr(_)) = (mem, nextAddr, e)
    |   step mem nextAddr (e as LocExpr(_)) = (mem, nextAddr, e)
    |   step mem nextAddr (e as TrueExpr) = (mem, nextAddr, e)
    |   step mem nextAddr (e as FalseExpr) = (mem, nextAddr, e)
    |   step mem nextAddr (e as FunExpr(_,_,_,_,_)) = (mem, nextAddr, e)

    |   step mem nextAddr (OpExpr(FalseExpr,Equal,FalseExpr)) = (mem,nextAddr,TrueExpr)
    |   step mem nextAddr (OpExpr(FalseExpr,Equal,TrueExpr)) = (mem,nextAddr,FalseExpr)
    |   step mem nextAddr (OpExpr(TrueExpr,Equal,FalseExpr)) = (mem,nextAddr,FalseExpr)
    |   step mem nextAddr (OpExpr(TrueExpr,Equal,TrueExpr)) = (mem,nextAddr,TrueExpr)
    |   step mem nextAddr (OpExpr(NumExpr(n1),Equal,NumExpr(n2))) = (mem, nextAddr, (if n1 = n2 then TrueExpr else FalseExpr))
    |   step mem nextAddr (OpExpr(NumExpr(n1),Less,NumExpr(n2))) = (mem, nextAddr, (if n1 < n2 then TrueExpr else FalseExpr))
    |   step mem nextAddr (OpExpr(NumExpr(n1),Minus,NumExpr(n2))) = (mem, nextAddr, NumExpr(n1-n2))
    |   step mem nextAddr (OpExpr(NumExpr(n1),Plus,NumExpr(n2))) = (mem, nextAddr, NumExpr(n1+n2))
    |   step mem nextAddr (OpExpr(NumExpr(n1),Times,NumExpr(n2))) = (mem, nextAddr, NumExpr(n1*n2))
    |   step mem nextAddr (OpExpr(e1,oper,e2)) =
      (fn (mem,nextAddr,v1) => 
        (fn (mem,nextAddr,v2) => step mem nextAddr (OpExpr(v1,oper,v2))
          ) (step mem nextAddr e2) (* e2 -> v2 *)
        ) (step mem nextAddr e1) (* e1 -> v1 *)

    |   step mem nextAddr (IfExpr(e1,e2,e3)) =
      (fn (mem,nextAddr,TrueExpr) => step mem nextAddr e2
        | (mem,nextAddr,FalseExpr) => step mem nextAddr e3
        | (_,_,_) => raise cannotStep
        ) (step mem nextAddr e1)

    |   step mem nextAddr (NewRefExpr(e1)) =
      (fn (mem,nextAddr,v1) => step ((nextAddr,v1)::mem) (nextAddr+1) (LocExpr(nextAddr))
        ) (step mem nextAddr e1)
    |   step mem nextAddr (DerefExpr(e1)) =
      (fn (mem,nextAddr,v1 as LocExpr(l)) => (mem, nextAddr, memory(v1,mem))
        | (_,_,_) => raise cannotStep
        ) (step mem nextAddr e1)
    |   step mem nextAddr (UpdateExpr(LocExpr(l),e2)) =
      (fn (mem,nextAddr,v2) => ((l,v2)::mem, nextAddr, v2)
        ) (step mem nextAddr e2) (* e2 -> v2 *)
    |   step mem nextAddr (UpdateExpr(e1,e2)) =
      (fn (mem,nextAddr,v1) => step mem nextAddr (UpdateExpr(v1,e2))
        ) (step mem nextAddr e1) (* e1 -> v1 *)

    |   step mem nextAddr (ApplyExpr(v1 as FunExpr(name,param,pt,rt,body),e2)) =
      (fn (mem,nextAddr,v2) => step mem nextAddr (sub (sub body param v2) name v1) (* call *)
        ) (step mem nextAddr e2) (* e2 -> v2 *)
    |   step mem nextAddr (ApplyExpr(e1,e2)) =
      (fn (mem,nextAddr,v1) => step mem nextAddr (ApplyExpr(v1,e2))
        ) (step mem nextAddr e1) (* e1 -> v1 *)
    |   step _ _ _ = raise cannotStep
    
  in
    (* return final expression *)
    (fn (_,_,e) => e ) ( step mem addr expr )
  end;
  
  (* Yay! *)

