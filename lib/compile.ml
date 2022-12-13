https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
open Asm.Directive
open S_exp
open Util

type symtab =
  int Symtab.symtab

(** Constants used for tagging values at runtime. *)

let num_shift = 2

let num_mask = 0b11

let num_tag = 0b00

let bool_shift = 7

let bool_mask = 0b1111111

let bool_tag = 0b0011111

let heap_mask = 0b111

let pair_tag = 0b010

(** [operand_of_num x] returns the runtime representation of the number [x] as
    an operand for instructions *)
let operand_of_num : int -> operand =
  fun x ->
    Imm ((x lsl num_shift) lor num_tag)

(** [operand_of_bool b] returns the runtime representation of the boolean [b] as
    an operand for instructions *)
let operand_of_bool : bool -> operand =
  fun b ->
    Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let zf_to_bool =
  [ Mov (Reg Rax, Imm 0)
  ; Setz (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag)
  ]

let setl_bool =
  [ Mov (Reg Rax, Imm 0)
  ; Setl (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag)
  ]

let stack_address : int -> operand =
  fun index ->
    MemOffset (Imm index, Reg Rsp)

let ensure_num : operand -> s_exp -> directive list =
  fun op e ->
    let msg_label = gensym "emsg" in
    let continue_label = gensym "continue" in
    [ Mov (Reg R8, op)
    ; And (Reg R8, Imm num_mask)
    ; Cmp (Reg R8, Imm num_tag)
    ; Je continue_label

      (* This will read data from location [msg_label] into [Rdi] *)
    ; LeaLabel (Reg Rdi, msg_label)

    ; Jmp "lisp_error"
    ; Label msg_label

    (* This has the effect of injecting the string [string_of_s_exp e] into the
       resulting binary file; in other words, this instruction never gets
       executed by the CPU, but rather just stores data. *)
    ; DqString (string_of_s_exp e)

    ; Label continue_label
    ]

let ensure_pair : operand -> s_exp -> directive list =
  fun op e ->
    let msg_label = gensym "emsg" in
    let continue_label = gensym "continue" in
    [ Mov (Reg R8, op)
    ; And (Reg R8, Imm heap_mask)
    ; Cmp (Reg R8, Imm pair_tag)
    ; Je continue_label
    ; LeaLabel (Reg Rdi, msg_label)
    ; Jmp "lisp_error"
    ; Label msg_label
    ; DqString (string_of_s_exp e)
    ; Label continue_label
    ]

(** [compile_unary_primitive e prim] produces x86-64 instructions for the unary
    primitive operation named by [prim]; if [prim] isn't a valid unary
    operation, it raises an exception using the s-expression [e]. *)
let compile_unary_primitive : s_exp -> string -> directive list =
  fun e prim ->
    begin match prim with
      | "add1" ->
          ensure_num (Reg Rax) e
            @ [Add (Reg Rax, operand_of_num 1)]

      | "sub1" ->
          ensure_num (Reg Rax) e
            @ [Sub (Reg Rax, operand_of_num 1)]

      | "zero?" ->
          [ Cmp (Reg Rax, operand_of_num 0)
          ]
          @ zf_to_bool

      | "num?" ->
          [ And (Reg Rax, Imm num_mask)
          ; Cmp (Reg Rax, Imm num_tag)
          ]
          @ zf_to_bool

      | "not" ->
          [ Cmp (Reg Rax, operand_of_bool false)
          ]
          @ zf_to_bool

      | "pair?" ->
          [ And (Reg Rax, Imm heap_mask)
          ; Cmp (Reg Rax, Imm pair_tag)
          ]
          @ zf_to_bool

      | "left" ->
          ensure_pair (Reg Rax) e
            @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag)))]

      | "right" ->
          ensure_pair (Reg Rax) e
            @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8)))]

      | _ ->
          raise (Error.Stuck e)
    end

(** [compile_binary_primitive stack_index e prim] produces x86-64 instructions
    for the binary primitive operation named by [prim] given a stack index of
    [stack_index]; if [prim] isn't a valid binary operation, it raises an error
    using the s-expression [e]. *)
let compile_binary_primitive : int -> s_exp -> string -> directive list =
  fun stack_index e prim ->
    begin match prim with
      | "+" ->
          ensure_num (Reg Rax) e
            @ ensure_num (stack_address stack_index) e
            @ [Add (Reg Rax, stack_address stack_index)]

      | "-" ->
          ensure_num (Reg Rax) e
            @ ensure_num (stack_address stack_index) e
            @ [ Mov (Reg R8, Reg Rax)
              ; Mov (Reg Rax, stack_address stack_index)
              ; Sub (Reg Rax, Reg R8)
              ]

      | "=" ->
          ensure_num (Reg Rax) e
            @ ensure_num (stack_address stack_index) e
            @ [Cmp (stack_address stack_index, Reg Rax)]
            @ zf_to_bool

      | "<" ->
          ensure_num (Reg Rax) e
            @ ensure_num (stack_address stack_index) e
            @ [Cmp (stack_address stack_index, Reg Rax)]
            @ setl_bool

      | "pair" ->
          [ Mov (Reg R8, stack_address stack_index)
          ; Mov (MemOffset (Reg Rdi, Imm 0), Reg R8)
          ; Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax)
          ; Mov (Reg Rax, Reg Rdi)
          ; Or (Reg Rax, Imm pair_tag)
          ; Add (Reg Rdi, Imm 16)
          ]

      | _ ->
          raise (Error.Stuck e)
    end

(** [compile_trinary_primitive stack_index e prim] produces x86-64 instructions
    for the trinary primitive operation named by [prim] given the stack index
    [stack_index]; if [prim] isn't a valid trinary operation, it raises an
    exception using the s-expression [e]. *)
let compile_trinary_primitive :
 int -> s_exp -> string -> directive list =
  fun stack_index e prim ->
    begin match prim with
      | _ ->
          raise (Error.Stuck e)
    end

let align : int -> int -> int =
  fun n alignment ->
    if n mod alignment = 0 then
      n
    else
      n + (alignment - (n mod alignment))

(** [compile_expr tab stack_index e] produces x86-64 instructions for the
    expression [e] given a symtab of [tab] and stack index of [stack_index]. *)
let rec compile_expr : symtab -> int -> s_exp -> directive list =
  fun tab stack_index e ->
    begin match e with
      | Num x ->
          [Mov (Reg Rax, operand_of_num x)]

      | Sym "true" ->
          [Mov (Reg Rax, operand_of_bool true)]

      | Sym "false" ->
          [Mov (Reg Rax, operand_of_bool false)]

      | Sym var when Symtab.mem var tab ->
          [Mov (Reg Rax, stack_address (Symtab.find var tab))]

      | Lst [Sym "if"; test_expr; then_expr; else_expr] ->
          let then_label = gensym "then" in
          let else_label = gensym "else" in
          let continue_label = gensym "continue" in
          compile_expr tab stack_index test_expr
            @ [Cmp (Reg Rax, operand_of_bool false)]
            @ [Je else_label]
            @ [Label then_label]
            @ compile_expr tab stack_index then_expr
            @ [Jmp continue_label]
            @ [Label else_label]
            @ compile_expr tab stack_index else_expr
            @ [Label continue_label]

      | Lst [Sym "let"; Lst [Lst [Sym var; exp]]; body] ->
          compile_expr tab stack_index exp
            @ [Mov (stack_address stack_index, Reg Rax)]
            @ compile_expr
                (Symtab.add var stack_index tab)
                (stack_index - 8)
                body

      | Lst (Sym "do" :: exps) when List.length exps > 0 ->
          List.concat_map (compile_expr tab stack_index) exps

      | Lst [Sym f; arg] as exp ->
          compile_expr tab stack_index arg
            @ compile_unary_primitive exp f

      | Lst [Sym f; arg1; arg2] as exp ->
          compile_expr tab stack_index arg1
            @ [Mov (stack_address stack_index, Reg Rax)]
            @ compile_expr tab (stack_index - 8) arg2
            @ compile_binary_primitive stack_index exp f

      | Lst [Sym f; arg1; arg2; arg3] as exp ->
          compile_expr tab stack_index arg1
            @ [Mov (stack_address stack_index, Reg Rax)]
            @ compile_expr tab (stack_index - 8) arg2
            @ [Mov (stack_address (stack_index - 8), Reg Rax)]
            @ compile_expr tab (stack_index - 16) arg3
            @ compile_trinary_primitive stack_index exp f

      | e ->
          raise (Error.Stuck e)
    end

(** [compile e] produces x86-64 instructions, including frontmatter, for the
    expression [e]. *)
let compile e =
  [ Global "lisp_entry"
  ; Extern "lisp_error"
  ; Section "text"
  ; Label "lisp_entry"
  ]
  @ compile_expr Symtab.empty (-8) e
  @ [Ret]
