open Core

exception Arithmetic_Error of string
exception Unrecognized_Opcode of string
exception Not_enough_op_args of string

type opcode =
  | AddInts
  | PushInt of int

let interpret_opcodes opcodes =
  let rec next (op_stack: int list) i =
    match Stream.peek opcodes, op_stack with
    | None, [] -> None
    | None, result :: tl -> Some result
    | Some op, op_stack -> (
      match op with
      | AddInts -> (
        match op_stack with
        | [] -> raise (Not_enough_op_args "not enough operator arguments")
        | one_op :: [] -> raise (Not_enough_op_args "not enough operator arguments")
        | a :: b :: tl ->
          Stream.junk opcodes;
          next ((a + b) :: tl) i
      )
      | PushInt op ->
        Stream.junk opcodes;
        next (op :: op_stack) i
    )
  in Stream.from (next [])

let consume_operand bytes =
  match Stream.peek bytes with
  | None -> raise (Not_enough_op_args "not enough operator arguments")
  | Some a ->
    Stream.junk bytes;
    a

let consume_operand_pair bytes =
  let a = consume_operand bytes in (a, consume_operand bytes)

let opcodes bytes =
  let next_opcode i =
    match Stream.peek bytes with
    | None -> None
    | Some 1 -> Stream.junk bytes; Some (PushInt (consume_operand bytes))
    | Some 2 -> Stream.junk bytes; Some AddInts
    | Some op -> raise (Unrecognized_Opcode (Printf.sprintf "couldn't recognize op: %i%!" op))
  in Stream.from(next_opcode)

let byte_word_stream_of_channel channel =
  Stream.from (fun _ -> In_channel.input_binary_int channel)
  
let interpret filename =
  In_channel.with_file filename ~f:(fun ic ->(
    Stream.iter (fun res -> print_int res; print_newline()) (interpret_opcodes (opcodes (byte_word_stream_of_channel ic)))
  ))

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
