open Miniml.Cui

let () =
  if Array.length Sys.argv = 1 then read_eval_print initial_env
  else batch_interpret Sys.argv.(1)
