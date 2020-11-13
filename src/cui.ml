open Eval

let rec get_latest_env xs =
  let _, newenv, _ = List.rev xs |> List.hd in
  newenv

let format_of_eval_error ((i : Syntax.pos), s) =
  let st, ed = i in
  if st.pos_lnum == ed.pos_lnum then
    Printf.sprintf "line %d characters %d-%d: %s\n" ed.pos_lnum
      (st.pos_cnum - st.pos_bol) (ed.pos_cnum - ed.pos_bol) s
  else
    Printf.sprintf "lines %d-%d, characters %d-%d: %s\n" st.pos_lnum ed.pos_lnum
      (st.pos_cnum - st.pos_bol) (ed.pos_cnum - ed.pos_bol) s

let print_single (id, _, v) =
  Printf.printf "val %s : " id;
  print_string " = ";
  pp_val v;
  print_newline ()

let print_output = List.iter print_single

let rec read_eval_print env =
  let aux () =
    print_string "# ";
    flush stdout;
    let decl = ParseUtil.parse (Lexing.from_channel stdin) in
    let output = eval_decl env decl in
    let newenv = get_latest_env output in
    print_output output;
    read_eval_print newenv
  in
  try aux () with
  | EvalError (i, s) ->
      format_of_eval_error (i, s) |> print_string;
      read_eval_print env
  | Failure s ->
      print_string s;
      print_newline ();
      read_eval_print env
  | Parser.Error ->
      print_string "Parsing error\n";
      read_eval_print env

let initial_env = Environment.empty

let aux env (lexbuf : Lexing.lexbuf) =
  try
    while not lexbuf.lex_eof_reached do
      let prog = ParseUtil.parse lexbuf in
      let output = eval_decl !env prog in
      let newenv = get_latest_env output in
      env := newenv;
      print_output output
    done
  with
  | Lexer.End_Of_File -> ()
  | EvalError (i, s) -> format_of_eval_error (i, s) |> print_string

let batch_interpret filename =
  let lexbuf = open_in filename |> Lexing.from_channel in
  let env = ref Environment.empty in
  aux env lexbuf
