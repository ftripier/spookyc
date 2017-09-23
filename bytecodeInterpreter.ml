open Core

exception Arithmetic_Error of string
exception Unrecognized_Opcode of string
exception Not_enough_op_args of string

let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
       try Some (input_line channel) with End_of_file -> None)

let interpret_chars chars =
  let rec next (op_stack: int list) i =
    match Stream.peek chars, op_stack with
    | None, [] -> None
    | None, [result] -> Some result
    | None, more :: than_one :: result -> Some more
    | Some '+', [one_result] -> raise (Not_enough_op_args "addition needs two operands - you gave one")
    | Some '+', [] -> raise (Not_enough_op_args "addition needs two operands - you gave zero")
    | Some '+', a :: b :: rest_of_ops ->
      Stream.junk chars;
      next ((a + b) :: rest_of_ops) i
    | Some '1', _ ->
      Stream.junk chars;
      let operand = Stream.peek chars in
      (match operand with
      | None -> raise (Not_enough_op_args "pushi needs an operand")
      | Some operand ->
        Stream.junk chars;
        next ((Char.to_int operand) :: op_stack) i)
    | Some op, _ ->
      raise (Unrecognized_Opcode (Printf.sprintf "couldn't recognize op: %s%!" (Char.escaped op))) in
  Stream.from (next [])

let interpret filename =
  let in_channel = open_in filename in
  let chars = Stream.of_channel in_channel in
  Stream.iter (
    fun result -> print_endline (string_of_int result)
  ) (interpret_chars chars)

let spec =
  let open Command.Spec in
  empty
  +> anon("filename" %: file)

let command =
  Command.basic
    ~summary:"Interpret some scary bytecode!"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> interpret filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command