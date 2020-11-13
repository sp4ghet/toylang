(* open AST

exception TypeError of string

let err s = raise (TypeError s)

type tyenv = tysc Environment.t

(* [] is no substitution *)
type subst = (tyvar * ty) list

let rec pp_subst = function
  | [] -> print_string "done\n"
  | (tyv, ty) :: rest ->
      print_char '(';
      print_string (string_of_tyvar tyv);
      print_char ',';
      pp_ty ty;
      print_string ") ";
      pp_subst rest

let rec freevar_tyenv tyenv =
  let rec aux tysc set =
    match tysc with
    | [], TyVar x -> MySet.insert x set
    | [], TyFun (t1, t2) -> MySet.union (aux ([], t1) set) (aux ([], t2) set)
    | [], _ -> set
    | a, ty -> MySet.union (MySet.from_list a) (aux ([], ty) set)
  in
  Environment.fold_right aux tyenv MySet.empty

let rec subst_type subst ty =
  match ty with
  | TyVar a ->
      let rec unpack = function
        | [] -> TyVar a
        | (x, t) :: rest ->
            if x = a then subst_type rest t else subst_type rest ty
      in
      unpack subst
  | TyFun (t1, t2) -> TyFun (subst_type subst t1, subst_type subst t2)
  | _ -> ty

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    MySet.bigunion
      (MySet.map (fun id -> freevar_ty (subst_type subst (TyVar id))) fv_tyenv')
  in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)

let rec eqs_of_subst s =
  match s with [] -> [] | (x, t) :: rest -> (TyVar x, t) :: eqs_of_subst rest

let rec subst_eqs s eqs =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest -> (subst_type s t1, subst_type s t2) :: subst_eqs s rest

let rec unify = function
  | [] -> []
  | (t1, t2) :: rest -> (
      if t1 = t2 then unify rest
      else
        match (t1, t2) with
        | TyFun (t11, t12), TyFun (t21, t22) ->
            unify ((t11, t21) :: (t12, t22) :: rest)
        | TyVar a, t ->
            let ftv = freevar_ty t in
            if MySet.member a ftv then err "occur check"
            else (a, t) :: (subst_eqs [ (a, t) ] rest |> unify)
        | t, TyVar a -> unify ((TyVar a, t) :: rest)
        | _ -> err "Unable to infer type" )

let check_multiple_decl ids id cont =
  if List.exists (fun x -> x = id) ids then
    err ("Variable " ^ id ^ " is bound several times in this matching")
  else cont

let ty_prim op t1 t2 =
  match op with
  | Plus -> ([ (t1, TyInt); (t2, TyInt) ], TyInt)
  | Mult -> ([ (t1, TyInt); (t2, TyInt) ], TyInt)
  | Lt -> ([ (t1, TyInt); (t2, TyInt) ], TyBool)
  | And -> ([ (t1, TyBool); (t2, TyBool) ], TyBool)
  | Or -> ([ (t1, TyBool); (t2, TyBool) ], TyBool)

let rec ty_exp tyenv = function
  | Var x -> (
      try
        let (TyScheme (vars, ty)) = Environment.lookup x tyenv in
        let s = List.map (fun id -> (id, fresh_tyvar ())) vars in
        ([], subst_type s ty)
      with Environment.Not_bound -> err ("variable not bound: " ^ x) )
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, e1, e2) ->
      let s1, tyarg1 = ty_exp tyenv e1 in
      let s2, tyarg2 = ty_exp tyenv e2 in
      let eqs3, t = ty_prim op tyarg1 tyarg2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 @ eqs3 in
      let s3 = unify eqs in
      (s3, subst_type s3 t)
  | IfExp (cond, e1, e2) ->
      let s1, tycond = ty_exp tyenv cond in
      let s2, ty1 = ty_exp tyenv e1 in
      let s3, ty2 = ty_exp tyenv e2 in
      let eq1 = s1 @ s2 @ s3 |> eqs_of_subst in
      let eqs = (tycond, TyBool) :: (ty1, ty2) :: eq1 in
      let s = unify eqs in
      (s, subst_type s ty1)
  | LetExp (xs, e2) ->
      let rec unpack env ids = function
        | [] -> ([], env)
        | (id, e) :: rest ->
            check_multiple_decl ids id ();
            let s, ty = ty_exp tyenv e in
            let actual_ty = subst_type s ty in
            let frees = freevar_ty actual_ty |> MySet.to_list in
            let new_tyenv =
              Environment.extend id (TyScheme (frees, actual_ty)) tyenv
            in
            let s1, nenv = unpack new_tyenv (id :: ids) rest in
            let eqs = eqs_of_subst (s @ s1) in
            let s = unify eqs in
            (s, nenv)
      in
      let s1, new_tyenv = unpack tyenv [] xs in
      let s2, ty = ty_exp new_tyenv e2 in
      let eqs = eqs_of_subst (s1 @ s2) in
      let s = unify eqs in
      let ty = subst_type s ty in
      (s, ty)
  | LetRecExp (xs, e2) ->
      let dummy_env = ref tyenv in
      let rec unpack env ids = function
        | [] -> ([], !env)
        | (id, p, e) :: rest ->
            check_multiple_decl ids id ();
            let arg_ty, ret_ty = (fresh_tyvar (), fresh_tyvar ()) in
            let new_env =
              Environment.extend p (tysc_of_ty arg_ty) !env
              |> Environment.extend id (tysc_of_ty (TyFun (arg_ty, ret_ty)))
            in
            env := new_env;
            let s1, ty = ty_exp new_env e in
            let s2, nenv = unpack env (id :: ids) rest in
            let s = (ret_ty, ty) :: eqs_of_subst (s1 @ s2) |> unify in
            (s, nenv)
      in
      let s1, new_tyenv = unpack dummy_env [] xs in
      let s2, ty = ty_exp new_tyenv e2 in
      let eqs = eqs_of_subst (s1 @ s2) in
      let s = unify eqs in
      let ty = subst_type s ty in
      (s, ty)
  | FunExp (id, exp) ->
      let domty = fresh_tyvar () in
      let s, bod_ty =
        ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp
      in
      (s, TyFun (subst_type s domty, bod_ty))
  | AppExp (f, e) ->
      let s1, fun_ty = ty_exp tyenv f in
      let s2, arg_ty = ty_exp tyenv e in
      let ty_bod = fresh_tyvar () in
      let s = s1 @ s2 in
      let eqs = (fun_ty, TyFun (arg_ty, ty_bod)) :: eqs_of_subst s in
      let s = unify eqs in
      let ty_bod = subst_type s ty_bod in
      (s, ty_bod)
  | _ -> err "Unknown token"

let rec type_decl ntyenv ptyenv ids = function
  | [] -> []
  | (x, e) :: rest ->
      check_multiple_decl ids x ();
      let _, e_t = ty_exp ptyenv e in
      let frees = freevar_ty e_t |> MySet.to_list in
      let new_env = Environment.extend x (TyScheme (frees, e_t)) ntyenv in
      (x, new_env, e_t) :: type_decl new_env ptyenv (x :: ids) rest

let rec type_rdecl tyenv ids = function
  | [] -> []
  | (id, p, e) :: rest ->
      check_multiple_decl ids id ();
      let arg_ty, ret_ty = (fresh_tyvar (), fresh_tyvar ()) in
      let new_env =
        Environment.extend id (TyScheme ([], TyFun (arg_ty, ret_ty))) !tyenv
        |> Environment.extend p (TyScheme ([], arg_ty))
      in
      let s1, ty = ty_exp new_env e in
      let s = (ret_ty, ty) :: eqs_of_subst s1 |> unify in
      let ty = subst_type s (TyFun (arg_ty, ret_ty)) in
      let nenv = Environment.extend id (TyScheme ([], ty)) !tyenv in
      tyenv := nenv;
      (id, nenv, ty) :: type_rdecl tyenv (id :: ids) rest

let rec ty_decl tyenv = function
  | Exp e ->
      let s, t = ty_exp tyenv e in
      [ ("-", tyenv, t) ]
  | Decl [] -> []
  | Decl (dec :: rest) ->
      let res = type_decl tyenv tyenv [] dec in
      let _, new_env, _ = List.rev res |> List.hd in
      res @ ty_decl new_env (Decl rest)
  | RecDecl decs ->
      let dummy_env = ref tyenv in
      type_rdecl dummy_env [] decs *)
