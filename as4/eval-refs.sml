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
    
    fun extract(_,_,e) = e
    
    (* substitution *)
    fun sub (e as NumExpr(_)) _ _ = e
    |   sub (e as LocExpr(_)) _ _ = e
    |   sub (e as TrueExpr) _ _ = e
    |   sub (e as FalseExpr) _ _ = e
    |   sub (e as VarExpr(s)) x v = if s = x then v else e
    |   sub (e as NewRefExpr(e1)) x v = NewRefExpr(sub e1 x v)
    |   sub (e as DerefExpr(e1)) x v = DerefExpr(sub e1 x v)
    |   sub (e as UpdateExpr(e1,e2)) x v = UpdateExpr((sub e1 x v),(sub e2 x v))
    |   sub (e as OpExpr(e1,oper,e2)) x v = OpExpr((sub e1 x v), oper, (sub e2 x v))
    |   sub (e as IfExpr(e1,e2,e3)) x v = IfExpr((sub e1 x v), (sub e2 x v), (sub e3 x v))
    |   sub (e as FunExpr(f,y,tp,tr,body)) x v =
          if x = f orelse x = y
            then FunExpr(f,y,tp,tr,body)
            else FunExpr(f,y,tp,tr,(sub body x v))
    |   sub (e as ApplyExpr(e1,e2)) x v = ApplyExpr((sub e1 x v), (sub e2 x v))
    
    (* memory lookup *)
    fun memory(L as LocExpr(loc), (l,e)::vals) = if loc = l then e else memory(L, vals)
    |   memory(_,_) = raise cannotStep
    
    (* dynamic semantics *)
    fun step mem nextL (e as NumExpr(_)) = (mem, nextL, e)
    |   step mem nextL (e as LocExpr(_)) = (mem, nextL, e)
    |   step mem nextL (e as TrueExpr) = (mem, nextL, e)
    |   step mem nextL (e as FalseExpr) = (mem, nextL, e)
    |   step mem nextL (e as FunExpr(_,_,_,_,_)) = (mem, nextL, e)
    |   step mem nextL (e as OpExpr(e1 as FalseExpr,oper,e2)) = ev (step mem nextL e2) e
    |   step mem nextL (e as OpExpr(e1 as TrueExpr,oper,e2)) = ev (step mem nextL e2) e
    |   step mem nextL (e as OpExpr(e1 as NumExpr(n1),oper,e2)) = ev (step mem nextL e2) e
    |   step mem nextL (e as OpExpr(e1,oper,e2)) = ev (step mem nextL e1) e
    |   step mem nextL (e as IfExpr(e1,e2,e3)) = ev (step mem nextL e1) e
    |   step mem nextL (e as NewRefExpr(e1)) = ev (step mem nextL e1) e
    |   step mem nextL (e as DerefExpr(e1)) = ev (step mem nextL e1) e
    |   step mem nextL (e as UpdateExpr(LocExpr(l),e2)) = ev (step mem nextL e2) e
    |   step mem nextL (e as UpdateExpr(e1,e2)) = ev (step mem nextL e1) e 
    |   step mem nextL (e as ApplyExpr(f as FunExpr(name,param,pt,rt,body),e2)) = ev (step mem nextL e2) e (* e2 -> v2 *)
    |   step mem nextL (e as ApplyExpr(e1,e2)) = ev (step mem nextL e1) e (* e1 -> v1 *)
    |   step _ _ _ = raise cannotStep
    
    (* more dynamic semantics *)
    and ev (mem,nextL,e2p as FalseExpr) (OpExpr(TrueExpr,Equal,e2)) = (mem, nextL, FalseExpr)
    |   ev (mem,nextL,e2p as FalseExpr) (OpExpr(FalseExpr,Equal,e2)) = (mem, nextL, TrueExpr)
    |   ev (mem,nextL,e2p as TrueExpr) (OpExpr(FalseExpr,Equal,e2)) = (mem, nextL, FalseExpr)
    |   ev (mem,nextL,e2p as TrueExpr) (OpExpr(TrueExpr,Equal,e2)) = (mem, nextL, TrueExpr)
    |   ev (mem,nextL,e2p as NumExpr(n2)) (OpExpr(NumExpr(n1),Equal,e2)) = (mem, nextL, (if n1 = n2 then TrueExpr else FalseExpr))
    |   ev (mem,nextL,e2p as NumExpr(n2)) (OpExpr(NumExpr(n1),Less,e2)) = (mem, nextL, (if n1 < n2 then TrueExpr else FalseExpr))
    |   ev (mem,nextL,e2p as NumExpr(n2)) (OpExpr(NumExpr(n1),Minus,e2)) = (mem, nextL, NumExpr(n1-n2))
    |   ev (mem,nextL,e2p as NumExpr(n2)) (OpExpr(NumExpr(n1),Times,e2)) = (mem, nextL, NumExpr(n1*n2))
    |   ev (mem,nextL,e2p as NumExpr(n2)) (OpExpr(NumExpr(n1),Plus,e2)) = (mem, nextL, NumExpr(n1+n2))
    |   ev (mem,nextL,e1p) (OpExpr(e1,oper,e2)) = step mem nextL (OpExpr(e1p,oper,e2))
    |   ev (mem,nextL,TrueExpr) (IfExpr(e1,e2,e3)) = step mem nextL e2 (* take true branch *)
    |   ev (mem,nextL,FalseExpr) (IfExpr(e1,e2,e3)) = step mem nextL e3 (* take false branch *)
    |   ev (mem,nextL,v1) (NewRefExpr(e1)) = step ((nextL,v1)::mem) (nextL+1) (LocExpr(nextL))
    |   ev (mem,nextL,v1 as LocExpr(l)) (DerefExpr(e1)) = (mem, nextL, memory(v1,mem))
    |   ev (mem,nextL,v1 as LocExpr(l)) (UpdateExpr(e1,e2)) = step mem nextL (UpdateExpr(v1,e2))
    |   ev (mem,nextL,v2) (UpdateExpr(v1 as LocExpr(l),e2)) = ((l,v2)::mem, nextL, v2)
    |   ev (mem,nextL,v2) (ApplyExpr(v1 as FunExpr(name,param,pt,rt,body),e2)) = step mem nextL (sub (sub body param v2) name v1) (* call *)
    |   ev (mem,nextL,v1 as FunExpr(name,param,pt,rt,body)) (ApplyExpr(e1,e2)) = step mem nextL (ApplyExpr(v1,e2)) (* e2 -> e2' *)
    |   ev _ _ = raise cannotStep
    
  in
    extract( step mem addr expr )
  end;
  
  (* Yay! *)

