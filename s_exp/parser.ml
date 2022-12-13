https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
open Stdlib

let parse (s : string) =
  let buf = Lexing.from_string s in
  Parse.main Lex.token buf

let parse_file file =
  let inx = open_in file in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parse.main Lex.token lexbuf in
  close_in inx ; ast
