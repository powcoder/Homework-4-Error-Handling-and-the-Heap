https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
open Core
open Lib

let command =
  Command.basic ~summary:"Interpret the given file"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Command.Param.string) in
      fun () ->
        try S_exp.parse_file filename |> Interp.interp |> printf "%s\n"
        with e -> Printf.eprintf "Error: %s\n" (Exn.to_string e))

let () = Command_unix.run ~version:"1.0" command
