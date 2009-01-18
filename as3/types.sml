(* types.sml *)
(* This file contains data types for MinML ASTs *)

(* MinML types *)
datatype typ = Int | Bool | Arrow of typ * typ;

(* MinML binary operators *)
datatype oper = Plus | Minus | Times | Less | Equal;

(* MinML expressions *)
datatype expr = VarExpr of string | NumExpr of int | OpExpr of expr*oper*expr
  | TrueExpr | FalseExpr | IfExpr of expr*expr*expr | ApplyExpr of expr*expr
  | FunExpr of string*string*typ*typ*expr;
  (* e.g.: FunExpr(funName, paramName, paramType, returnType, bodyExpr) *)

