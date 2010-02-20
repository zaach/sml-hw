(* types-refs.sml *)

(* lambda calculus expressions *)
datatype expr = VarExpr of string | ApplyExpr of expr*expr
  | FunExpr of string*expr;
  (* e.g.: FunExpr(paramName, bodyExpr) *)

