(* types-refs.sml *)
(* This file contains data types for MinML ASTs, including references *)

(* MinML types *)
datatype typ = Int | Bool | Arrow of typ * typ | Ref of typ; 

(* MinML binary operators *)
datatype oper = Plus | Minus | Times | Less | Equal;

(* MinML expressions *)
datatype expr = VarExpr of string | NumExpr of int | OpExpr of expr*oper*expr
  | TrueExpr | FalseExpr | IfExpr of expr*expr*expr | ApplyExpr of expr*expr
  | LocExpr of int (* e.g., a location l (where l is a memory address) *)
  | NewRefExpr of expr (* e.g., ref(e) *)
  | DerefExpr of expr (* e.g., !e *)
  | UpdateExpr of expr*expr (* e.g., e1 := e2 *)
  | FunExpr of string*string*typ*typ*expr;
  (* e.g.: FunExpr(funName, paramName, paramType, returnType, bodyExpr) *)

