https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
open S_exp

exception Stuck of s_exp

let () =
  Printexc.register_printer (function
    | Stuck e ->
        Some (Printf.sprintf "Stuck[%s]" (string_of_s_exp e))
    | _ ->
        None)
