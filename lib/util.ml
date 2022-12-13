https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
open S_exp

let gensym : string -> string =
  let counter = ref 0 in
  fun (base : string) ->
    let number = !counter in
    counter := !counter + 1 ;
    Printf.sprintf "%s__%d" base number

let get_bindings (lst : s_exp list) : (string * s_exp) list =
  List.map
    (fun (e : s_exp) ->
      match e with
      | Lst [Sym var; exp] ->
          (var, exp)
      | _ ->
          raise (Error.Stuck e))
    lst

let get_cases (lst : s_exp list) : (int * s_exp) list =
  List.map
    (fun (e : s_exp) ->
      match e with Lst [Num n; exp] -> (n, exp) | _ -> raise (Error.Stuck e))
    lst

module List = struct
  include List

  let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi
end
