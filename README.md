# Toylang

This language is basically a mini OCaml with very limited features.
There is a work in progress VSCode extension at https://github.com/sp4ghet/vsc-ext

```ocaml
let f x y = x * y;;

(* this is a comment *)
let g x = if x < 0 then false else true;;

let h x = if x > 0 then
    true
else
    false
;;

let hoge = if true then
    false
else
    true
;;


let x = h 5 && g 3;;

let and a b = a && b;;
let or a b = a || b;;

f 5 4;;

f 5 true;;
(2 + 3);;
```
