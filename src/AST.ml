type id = string

type binOp = Plus | Mult | Minus | Lt | Gt | And | Or

type raw_exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp

and exp = (Lexing.position * Lexing.position) * raw_exp

type program = Exp of exp | Decl of id * exp

type tyvar = int

type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty | TyList of ty

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)

let freevar_tysc tysc =
  let rec aux set = function
    | [], TyVar x -> MySet.insert x set
    | [], TyFun (t1, t2) -> MySet.union (aux set ([], t1)) (aux set ([], t2))
    | [], _ -> set
    | x, ty ->
        MySet.union (MySet.from_list x) set |> MySet.union (aux set ([], ty))
  in
  aux MySet.empty tysc

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyVar v
  in
  body

let rec freevar_ty ty =
  let rec aux set = function
    | TyVar x -> MySet.insert x set
    | TyFun (t1, t2) -> MySet.union (aux set t1) (aux set t2)
    | _ -> set
  in
  aux MySet.empty ty

let string_of_tyvar n =
  let letter = int_of_char 'a' + (n mod 26) and suffix = n / 26 in
  Printf.sprintf "'%c" (char_of_int letter)
  ^ if suffix = 0 then "" else string_of_int suffix

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVar x -> string_of_tyvar x
  | TyFun (t1, t2) -> (
      match (t1, t2) with
      | TyFun _, TyFun _ ->
          "(" ^ string_of_ty t1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty t2 ^ ")"
      | TyFun _, _ -> "(" ^ string_of_ty t1 ^ ")" ^ " -> " ^ string_of_ty t2
      | _, TyFun _ -> string_of_ty t1 ^ " -> (" ^ string_of_ty t2 ^ ")"
      | _ -> string_of_ty t1 ^ " -> " ^ string_of_ty t2 )
  | _ -> invalid_arg "unsupported type"

let pp_ty ty = print_string (string_of_ty ty)
