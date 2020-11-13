open AST

type exval =
  | IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref

and dnval = exval

exception EvalError of Syntax.pos * string

let err (i : Syntax.pos) s = EvalError (i, s) |> raise

(* pretty printing *)
let rec string_of_exp (_, e) =
  match e with
  | Var id -> id
  | ILit n -> string_of_int n
  | BLit b -> string_of_bool b
  | BinOp (Plus, e1, e2) ->
      "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | BinOp (Minus, e1, e2) ->
      "(" ^ string_of_exp e1 ^ " - " ^ string_of_exp e2 ^ ")"
  | BinOp (Mult, e1, e2) -> string_of_exp e1 ^ " * " ^ string_of_exp e2
  | BinOp (Lt, e1, e2) -> string_of_exp e1 ^ " < " ^ string_of_exp e2
  | BinOp (Gt, e1, e2) -> string_of_exp e1 ^ ">" ^ string_of_exp e2
  | BinOp (And, e1, e2) -> string_of_exp e1 ^ " && " ^ string_of_exp e2
  | BinOp (Or, e1, e2) -> string_of_exp e1 ^ " || " ^ string_of_exp e2
  | IfExp (cond, e1, e2) ->
      "if " ^ string_of_exp cond ^ " then " ^ string_of_exp e1 ^ " else "
      ^ string_of_exp e2
  | LetExp (x, e1, e2) ->
      "let " ^ x ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
  | FunExp (id, e) -> "fun " ^ id ^ " -> " ^ string_of_exp e
  | AppExp (f, x) -> string_of_exp f ^ " " ^ string_of_exp x

let rec string_of_exval = function
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (id, exp, _) -> "fun " ^ id ^ " -> " ^ string_of_exp exp

let pp_val v = print_string (string_of_exval v)

let rec apply_prim i op arg1 arg2 =
  match (op, arg1, arg2) with
  | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err i "Both arguments must be integer: +"
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err i "Both arguments must be integer: -"
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err i "Both arguments must be integer: *"
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err i "Both arguments must be integer: <"
  | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
  | Gt, _, _ -> err i "Both arguments must be integer: >"
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err i "Both arguments must be boolean: &&"
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err i "Both arguments must be boolean: ||"

let rec eval_exp env (i, e) =
  match e with
  | Var x -> (
      try Environment.lookup x env
      with Environment.Not_bound -> err i ("Variable not bound: " ^ x) )
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> (
      let arg1 = eval_exp env exp1 in
      match (op, arg1) with
      (* early return *)
      | And, BoolV false -> BoolV false
      | Or, BoolV true -> BoolV true
      | _ ->
          let arg2 = eval_exp env exp2 in
          apply_prim i op arg1 arg2 )
  | IfExp (exp1, exp2, exp3) -> (
      let test = eval_exp env exp1 in
      match test with
      | BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> err i "Test expression must be boolean: if" )
  | LetExp (x, e1, e2) ->
      let value = eval_exp env e1 in
      let newenv = Environment.extend x value env in
      eval_exp newenv e2
  | FunExp (id, e) -> ProcV (id, e, ref env)
  | AppExp (f, e) -> (
      let funval = eval_exp env f in
      let arg = eval_exp env e in
      match funval with
      | ProcV (id, exp, env') -> (
          let newenv = Environment.extend id arg !env' in
          try eval_exp newenv exp with EvalError (_, s) -> err i s )
      | _ -> err i "Applying argument to non-function" )

let rec eval_decl env = function
  | Exp e ->
      let v = eval_exp env e in
      [ ("-", env, v) ]
  | Decl (id, exp) ->
      let eval = eval_exp env exp in
      let nenv = Environment.extend id eval env in
      [ (id, nenv, eval) ]
