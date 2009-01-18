(* 
    Programming Languages - Fall 2008 - J. Ligatti
    Assignment 3
    
    MinML Type Checker

    I pledge my Honor that I have not cheated on this assignment.
    - Zachary Carter
*) 

fun tc(expr) = 
  let
    val vartypes = []
    
    val SI = SOME Int
    
    (* loop through var*type list to find var of name and return type *)
    fun vartype(name:string, (v,t)::vartypes) = if name = v then SOME(t) else vartype(name, vartypes)
    |   vartype(_, nil) = NONE
    
    (* do the easy stuff first *)
    fun tc(VarExpr(v),vts) = vartype(v, vts)
    |   tc(TrueExpr,_) = SOME Bool
    |   tc(FalseExpr,_) = SOME Bool
    |   tc(NumExpr(_),_) = SOME Int
    |   tc(OpExpr(e1,Plus,e2), vts) = if tc(e1, vts) = SI andalso tc(e2, vts) = SI then SI else NONE
    |   tc(OpExpr(e1,Minus,e2), vts) = if tc(e1, vts) = SI andalso tc(e2, vts) = SI then SI else NONE
    |   tc(OpExpr(e1,Times,e2), vts) = if tc(e1, vts) = SI andalso tc(e2, vts) = SI then SI else NONE
    |   tc(OpExpr(e1,Less,e2), vts) = if tc(e1, vts) = SI andalso tc(e2, vts) = SI then SOME Bool else NONE
    |   tc(OpExpr(e1,Equal,e2),vts) =
          if tc(e1,vts) = tc(e2,vts) andalso
            ( tc(e1,vts) = SOME Int orelse tc(e1,vts) = SOME Bool )
            then SOME Bool else NONE
    |   tc(IfExpr(e1, e2, e3), vts) = if tc(e1, vts) = SOME Bool andalso tc(e2, vts) = tc(e3, vts) then tc(e1, vts) else NONE
    (* for functions, "f(x:pt):rt is body end", check body type is rt *)
    |   tc(FunExpr(f,x,pt,rt,body), vts) =
          if tc(body, (f,Arrow(pt,rt))::(x,pt)::vts) = SOME(rt) then
              SOME(Arrow(pt,rt)) else NONE
    (* for apply, check that input type of fun is same type of e2 *)
    |   tc(ApplyExpr(e1,e2), vts) =
          (fn (SOME(Arrow(pt,rt)),et) => if SOME pt = et then SOME rt else NONE | (_,_) => NONE )(tc(e1,vts), tc(e2,vts))
  in
    tc(expr, vartypes)
  end;

