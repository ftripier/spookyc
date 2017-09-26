open Core

exception Arithmetic_Error of string
exception Unrecognized_Opcode of string
exception Not_enough_op_args of string
exception What_r_u_doing_lol of string

type binary_operation =
  | AddInts
  | DivideInts
  | MultiplyInts
  | SubtractInts

type unary_operation =
  | Negation

type opcode =
  | PushInt of int
  | BinaryOperation of binary_operation
  | LoadLocal of int
  | StoreLocal of int
  | UnaryOperation of unary_operation
  | FunctionDeclaration of function_declaration
  | EndFunctionDeclaration
  | FunctionCall of int
  | Return
and function_declaration = {
  symbol: int;
  num_parameters: int;
  num_locals: int;
  op_codes: opcode list;
}

class virtual_machine = object(self)
  val mutable functions = Int.Table.create()
  val mutable registers = (Array.create ~len:0 0: int array)
  val mutable op_stack = ([] : int list)

  method set_function function_dec =
    Hashtbl.set functions ~key:function_dec.symbol ~data:function_dec

  method binary_op op =
    match op_stack with
    | [] -> raise (Not_enough_op_args "not enough operator arguments")
    | one_op :: [] -> raise (Not_enough_op_args "not enough operator arguments")
    | a :: b :: tl -> let result = (
        match op with
        | AddInts -> a + b
        | DivideInts -> a / b
        | MultiplyInts -> a * b
        | SubtractInts -> a - b
      ) in (result :: tl)
  
  method unary_op op =
    match op_stack with
    | [] -> raise (Not_enough_op_args "not enough operator arguments. You just needed one man, come on.")
    | a :: tl -> let result = (
      match op with
      | Negation -> -a
    ) in (result :: tl)
  
  method push_arguments num_args =
    let arguments = Array.create ~len:num_args 0 in
    let rec pop args_left =
      let index = num_args - args_left in 
      if args_left == 0 then () else (
      match op_stack with
      | [] -> raise (Not_enough_op_args "Just one operand oh my god it's not that hard.")
      | a :: tl ->
        op_stack <- tl;
        Array.set arguments index a;
        pop (args_left - 1)
      ) in
    pop num_args;
    registers <- arguments

  method interpret_opcodes opcodes =
    match Stream.peek opcodes, op_stack with
    | None, [] -> None
    | None, result :: tl -> Some result
    | Some op, _ -> (
      match op with
      | BinaryOperation op ->
        Stream.junk opcodes;
        op_stack <- (self#binary_op op);
        self#interpret_opcodes opcodes
      | UnaryOperation op ->
        Stream.junk opcodes;
        op_stack <- (self#unary_op op);
        self#interpret_opcodes opcodes
      | PushInt op ->
        Stream.junk opcodes;
        op_stack <- (op :: op_stack);
        self#interpret_opcodes opcodes
      | LoadLocal op ->
        Stream.junk opcodes;
        op_stack <- (Array.get registers op) :: op_stack;
        self#interpret_opcodes opcodes
      | StoreLocal op ->
        Stream.junk opcodes;
        (match op_stack with
          | [] -> raise (Not_enough_op_args "Oh no! A fairy thief stole the only argument you were supposed to pass to the storeLocal op!")
          | a :: tl ->
            Array.set registers op a;
            self#interpret_opcodes opcodes
        )
      | Return ->
        Stream.junk opcodes;
        (match op_stack with
        | [] -> None
        | a :: tl -> Some a
      )
      | FunctionDeclaration op ->
        self#set_function op;
        self#interpret_opcodes opcodes
      | EndFunctionDeclaration -> self#interpret_opcodes opcodes
      | FunctionCall op ->
        let called = Hashtbl.find functions op in
        match called with
        | None -> raise (What_r_u_doing_lol "can't call a function before you define it")
        | Some called ->
        let old_registers = registers in
        let old_op_stack = op_stack in
        registers <- (Array.create ~len:0 0 : int array);
        self#push_arguments called.num_parameters;
        let call_result = self#interpret_opcodes (Stream.of_list called.op_codes) in
        op_stack <- old_op_stack;
        registers <- old_registers;
        call_result
    )
end

let consume_operand bytes =
  match Stream.peek bytes with
  | None -> raise (Not_enough_op_args "not enough operator arguments")
  | Some a ->
    Stream.junk bytes;
    a

let consume_operand_pair bytes =
  let a = consume_operand bytes in (a, consume_operand bytes)

let rec buffer_opcodes ?b:(buffered=[]) ops =
  match Stream.peek ops with
  | None -> buffered
  | Some a ->
    Stream.junk ops;
    buffer_opcodes ~b:(a :: buffered) ops

let rec opcodes bytes =
  let next_opcode i =
    match Stream.peek bytes with
    | None -> None
    | Some 1 -> Stream.junk bytes; Some (PushInt (consume_operand bytes))
    | Some 2 -> Stream.junk bytes; Some (BinaryOperation(AddInts))
    | Some 3 -> Stream.junk bytes; Some (BinaryOperation(SubtractInts))
    | Some 4 -> Stream.junk bytes; Some (BinaryOperation(MultiplyInts))
    | Some 5 -> Stream.junk bytes; Some (BinaryOperation(DivideInts))
    | Some 6 -> Stream.junk bytes; Some (LoadLocal (consume_operand bytes))
    | Some 7 -> Stream.junk bytes; Some (StoreLocal (consume_operand bytes))
    | Some 8 ->
      Stream.junk bytes;
      Some (FunctionDeclaration {
        symbol = consume_operand bytes;
        num_parameters = consume_operand bytes;
        num_locals = consume_operand bytes;
        op_codes = (buffer_opcodes (opcodes bytes));
      })
    | Some 9 -> Stream.junk bytes; None
    | Some 10 -> Stream.junk bytes; Some (FunctionCall (consume_operand bytes))
    | Some 11 -> Stream.junk bytes; Some (Return)
    | Some 12 -> Stream.junk bytes; Some (UnaryOperation(Negation))
    | Some op -> raise (Unrecognized_Opcode (Printf.sprintf "couldn't recognize op: %i%!" op))
  in Stream.from(next_opcode)

let byte_word_stream_of_channel channel =
  Stream.from (fun _ -> In_channel.input_binary_int channel)
  
let interpret filename =
  let vm = new virtual_machine in
  In_channel.with_file filename ~f:(fun ic ->(
    let res = vm#interpret_opcodes (opcodes (byte_word_stream_of_channel ic)) in
    match res with
    | None -> print_endline "No result!"
    | Some res ->
      print_int res;
      print_newline()
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
