open Core

exception Arithmetic_Error of string
exception Unrecognized_Opcode of string
exception Not_enough_op_args of string
exception What_r_u_doing_lol of string

type binary_operation =
  | AddNumeric
  | DivideNumeric
  | MultiplyNumeric
  | SubtractNumeric

type unary_operation =
  | Negation

type opcode =
  | PushNumeric of float
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

let apply_constant_unary_op a op =
  match a, op with
  | PushNumeric a, UnaryOperation op -> (
    match op with
    | Negation -> [PushNumeric(~-. a)]
  )
  | _, _ -> [op; a]

let apply_constant_binary_op a b op =
  match a, b, op with
  | PushNumeric a, PushNumeric b, BinaryOperation op -> (
    match op with
    | AddNumeric -> [PushNumeric (a +. b)]
    | DivideNumeric -> [PushNumeric (a /. b)]
    | MultiplyNumeric -> [PushNumeric (a *. b)]
    | SubtractNumeric -> [PushNumeric (a -. b)]
  )
  | _, _, _ -> [op; a; b]

let constant_folding op_codes =
  let constant_folded = List.fold_left op_codes ~f:(fun optimized op -> 
    match optimized, op with
    | [], op -> [op]
    | a :: [], op -> apply_constant_unary_op a op
    | a :: b :: tl, op -> (
      match op, a, b with
      | UnaryOperation unop, PushNumeric inta, _ ->
        List.append (apply_constant_unary_op a op) (b :: tl)
      | BinaryOperation binop, PushNumeric inta, PushNumeric intb ->
        List.append (apply_constant_binary_op a b op) tl
      | _, _, _ -> (op :: a :: b :: tl)
    )
  ) ~init:([]:opcode list) in List.rev constant_folded

let optimize_ops op_codes =
  constant_folding op_codes

class virtual_machine = object(self)
  val mutable functions = Int.Table.create()
  val mutable registers = ((Array.create ~len:0 0.0): float array)
  val mutable op_stack = ([] : float list)

  method set_function function_dec =
    Hashtbl.set functions ~key:function_dec.symbol ~data:function_dec

  method binary_op op =
    match op_stack with
    | [] -> raise (Not_enough_op_args "not enough operator arguments")
    | one_op :: [] -> raise (Not_enough_op_args "not enough operator arguments")
    | a :: b :: tl -> let result = (
        match op with
        | AddNumeric -> a +. b
        | DivideNumeric -> a /. b
        | MultiplyNumeric -> a *. b
        | SubtractNumeric -> a -. b
      ) in (result :: tl)
  
  method unary_op op =
    match op_stack with
    | [] -> raise (Not_enough_op_args "not enough operator arguments. You just needed one man, come on.")
    | a :: tl -> let result = (
      match op with
      | Negation -> ~-. a
    ) in (result :: tl)
  
  method push_arguments num_args num_locals =
    print_int num_args;
    print_newline();
    let arguments = Array.create ~len:(num_args + num_locals) 0.0 in
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
        print_endline "BinOp";
        Stream.junk opcodes;
        op_stack <- (self#binary_op op);
        self#interpret_opcodes opcodes
      | UnaryOperation op ->
        print_endline "UnOp";      
        Stream.junk opcodes;
        op_stack <- (self#unary_op op);
        self#interpret_opcodes opcodes
      | PushNumeric op ->
        print_string "PushNumeric: ";
        print_float op;
        print_newline();
        Stream.junk opcodes;
        op_stack <- (op :: op_stack);
        self#interpret_opcodes opcodes
      | LoadLocal op ->
        Printf.printf "LoadLocal: %d\n%!" op;      
        Stream.junk opcodes;
        op_stack <- (Array.get registers op) :: op_stack;
        self#interpret_opcodes opcodes
      | StoreLocal op ->
        Printf.printf "StoreLocal: %d\n%!" op;
        Stream.junk opcodes;
        (match op_stack with
          | [] -> raise (Not_enough_op_args "Oh no! A fairy thief stole the only argument you were supposed to pass to the storeLocal op!")
          | a :: tl ->
            Array.set registers op a;
            self#interpret_opcodes opcodes
        )
      | Return ->
        print_endline "Return";
        Stream.junk opcodes;
        (match op_stack with
        | [] -> None
        | a :: tl -> Some a
      )
      | FunctionDeclaration op ->
        print_endline "FunctionDec";
        Stream.junk opcodes;      
        self#set_function op;
        self#interpret_opcodes opcodes
      | EndFunctionDeclaration ->
        print_endline "EndFunctionDec";
        Stream.junk opcodes;      
        self#interpret_opcodes opcodes
      | FunctionCall op ->
        Printf.printf "FunctionCall: %d\n%!" op;
        Stream.junk opcodes;      
        let called = Hashtbl.find functions op in
        match called with
        | None -> raise (What_r_u_doing_lol "can't call a function before you define it")
        | Some called ->
        let old_registers = registers in
        let old_op_stack = op_stack in
        registers <- (Array.create ~len:0 0.0);
        self#push_arguments called.num_parameters called.num_locals;
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

let consume_float bytes =
  let top_bits = Int64.shift_left (Int64.of_int32 (consume_operand bytes)) 32 in
  let bottom_bits = Int64.of_int32 (consume_operand bytes) in
  Int64.float_of_bits (Int64.bit_or top_bits bottom_bits)

let consume_operand_pair bytes =
  let a = consume_operand bytes in (a, consume_operand bytes)

let rec buffer_opcodes ?b:(buffered=[]) ops =
  match Stream.peek ops with
  | None -> List.rev buffered
  | Some a ->
    Stream.junk ops;
    buffer_opcodes ~b:(a :: buffered) ops

let rec opcodes bytes =
  let next_opcode i =
    match Stream.peek bytes with
    | None -> None
    | Some num -> (
      match Int32.to_int_exn num with
        | 1 -> Stream.junk bytes; Some (PushNumeric (consume_float bytes))
        | 2 -> Stream.junk bytes; Some (BinaryOperation(AddNumeric))
        | 3 -> Stream.junk bytes; Some (BinaryOperation(SubtractNumeric))
        | 4 -> Stream.junk bytes; Some (BinaryOperation(MultiplyNumeric))
        | 5 -> Stream.junk bytes; Some (BinaryOperation(DivideNumeric))
        | 6 -> Stream.junk bytes; Some (LoadLocal (Int32.to_int_exn (consume_operand bytes)))
        | 7 -> Stream.junk bytes; Some (StoreLocal (Int32.to_int_exn (consume_operand bytes)))
        | 8 ->
          Stream.junk bytes;
          let symbol = Int32.to_int_exn (consume_operand bytes) in
          let num_parameters = Int32.to_int_exn (consume_operand bytes) in
          let num_locals = Int32.to_int_exn (consume_operand bytes) in
          let op_codes = optimize_ops (buffer_opcodes (opcodes bytes)) in
          Some (FunctionDeclaration {
            symbol;
            num_parameters;
            num_locals;
            op_codes;
          })
        | 9 -> Stream.junk bytes; None
        | 10 -> Stream.junk bytes; Some (FunctionCall (Int32.to_int_exn (consume_operand bytes)))
        | 11 -> Stream.junk bytes; Some (Return)
        | 12 -> Stream.junk bytes; Some (UnaryOperation(Negation))
        | op -> raise (Unrecognized_Opcode (Printf.sprintf "couldn't recognize op: %i%!" op))
    )
  in Stream.from(next_opcode)

let interpret bytestream =
  let vm = new virtual_machine in
  let res = vm#interpret_opcodes (opcodes bytestream) in
  match res with
  | None -> print_endline "No result!"
  | Some res ->
    print_float res;
    print_newline()
