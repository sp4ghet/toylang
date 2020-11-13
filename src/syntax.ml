module A = AST

type id = string

type binOp = A.binOp

type pos = Lexing.position * Lexing.position

type exp =
  | Var' of pos * id
  | ILit' of pos * int
  | BLit' of pos * bool
  | BinOp' of pos * binOp * exp * exp
  | IfExp' of pos * exp * exp * exp
  | LetExp' of pos * id * exp * exp
  | FunExp' of pos * id * exp
  | AppExp' of pos * exp * exp

type program = Exp of pos * exp | Decl of pos * (id * exp)

let tag pos e = (pos, e)

let rec simplify_exp = function
  | Var' (pos, id) -> A.Var id |> tag pos
  | ILit' (pos, x) -> A.ILit x |> tag pos
  | BLit' (pos, b) -> A.BLit b |> tag pos
  | BinOp' (pos, op, l, r) ->
      A.BinOp (op, simplify_exp l, simplify_exp r) |> tag pos
  | IfExp' (pos, cond, tr, fl) ->
      A.IfExp (simplify_exp cond, simplify_exp tr, simplify_exp fl) |> tag pos
  | LetExp' (pos, x, e1, e2) ->
      A.LetExp (x, simplify_exp e1, simplify_exp e2) |> tag pos
  | FunExp' (pos, x, e) -> A.FunExp (x, simplify_exp e) |> tag pos
  | AppExp' (pos, f, v) -> A.AppExp (simplify_exp f, simplify_exp v) |> tag pos
