open Syntax
module A = AST

let parse lexbuf =
  let surface = Parser.toplevel Lexer.main lexbuf in
  let prog =
    match surface with
    | Exp (_, e) -> A.Exp (simplify_exp e)
    | Decl (_, (x, e)) -> A.Decl (x, simplify_exp e)
  in
  prog
