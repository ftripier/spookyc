open Core

exception What_r_u_doing_lol of string

type binary_operation =
  | Add
  | DivideNumeric
  | MultiplyNumeric
  | SubtractNumeric

type spookyval =
  | Numeric of float
  | Spookystring of string
  | Void

type unary_operation =
  | Negation

type opcode =
  | PushSpookyvalue of spookyval
  | BinaryOperation of binary_operation
  | LoadLocal of int
  | StoreLocal of int
  | UnaryOperation of unary_operation
  | FunctionDeclaration of function_declaration
  | EndFunctionDeclaration
  | FunctionCall of int
  | CallBuiltin of int
  | Return
and function_declaration = {
  symbol: int;
  num_parameters: int;
  num_locals: int;
  op_codes: opcode list;
}

let print_spookyval spval =
  match spval with
  | Numeric sp ->
    print_float sp;
    print_string " "
  | Spookystring sp ->
    print_string sp;
    print_string " "
  | Void ->
    print_string "Void";
    print_string " "    

let apply_unary_op a op =
  match a with
  | Numeric anum -> (
    match op with
    | Negation -> (Numeric(~-. anum))
  )
  | Spookystring anum -> raise (What_r_u_doing_lol "Negating strings makes me fear puke!")
  | Void -> Void

let apply_binary_op a b op =
  match a, b with
  | Numeric a, Numeric b -> (
    match op with
    | Add -> (Numeric(a +. b))
    | DivideNumeric -> (Numeric(a /. b))
    | MultiplyNumeric -> (Numeric(a *. b))
    | SubtractNumeric -> (Numeric(a -. b))
  )
  | Numeric a, Spookystring b -> (
    match op with
    | Add -> (Spookystring ((string_of_float a) ^ b))
    | _ -> raise (What_r_u_doing_lol "AHHHHHHhhhhHHHHHHHhhhHHHHH! You can't use that operator on a string!!!")
  )
  | Spookystring a, Numeric b -> (
    match op with
    | Add -> (Spookystring (a ^ (string_of_float b)))
    | _ -> raise (What_r_u_doing_lol "AHHHHHHhhhhHHHHHHHhhhHHHHH! You can't use that operator on a string!!!")
  )
  | Spookystring a, Spookystring b -> (
    match op with
    | Add -> (Spookystring (a ^ b))
    | _ -> raise (What_r_u_doing_lol "AHHHHHHhhhhHHHHHHHhhhHHHHH! You can't use that operator on a string!!!")
  )
  | _, _ -> Void

let apply_constant_unary_op a op =
  PushSpookyvalue (apply_unary_op a op)

let apply_constant_binary_op a b op =
  PushSpookyvalue (apply_binary_op a b op)

let constant_folding op_codes =
  let constant_folded = List.fold_left op_codes ~f:(fun optimized op -> 
    match optimized, op with
    | [], op -> [op]
    | a :: [], op -> (
      match op, a with
      | UnaryOperation unop, PushSpookyvalue vala -> [apply_constant_unary_op vala unop]
      | _, _ -> (op :: optimized)
    )
    | a :: b :: tl, op -> (
      match op, a, b with
      | UnaryOperation unop, PushSpookyvalue vala, _ ->
        (apply_constant_unary_op vala unop) :: b :: tl
      | BinaryOperation binop, PushSpookyvalue vala, PushSpookyvalue valb ->
        (apply_constant_binary_op vala valb binop) :: tl
      | _, _, _ -> (op :: optimized)
    )
  ) ~init:([]:opcode list) in List.rev constant_folded

let optimize_ops op_codes =
  constant_folding op_codes

class virtual_machine = object(self)
  val mutable functions = Int.Table.create()
  val mutable registers = ((Array.create ~len:0 (Numeric(0.0))): spookyval array)
  val mutable op_stack = ([] : spookyval list)

  method interpreter_scream =
    match op_stack with
      | [] -> print_endline "AHHHHHHHHHHHH!"
      | a :: tl ->
        print_spookyval a;
        print_endline "AHHHHHHHHHHHH!";
        op_stack <- tl

  method creppy_whispers_from_outside =
    let very_creppy = read_line() in
      op_stack <- (Spookystring(very_creppy) :: op_stack)

  method print_registers =
    print_string "registers = ";
    Array.iter ~f:print_spookyval registers;
    print_newline()

  method set_function function_dec =
    Hashtbl.set functions ~key:function_dec.symbol ~data:function_dec

  method binary_op op =
    match op_stack with
    | [] -> raise (What_r_u_doing_lol "not enough operator arguments")
    | one_op :: [] -> raise (What_r_u_doing_lol "not enough operator arguments")
    | a :: b :: tl -> let result = (apply_binary_op a b op) in (result :: tl)
  
  method unary_op op =
    match op_stack with
    | [] -> raise (What_r_u_doing_lol "not enough operator arguments. You just needed one man, come on.")
    | a :: tl -> let result = (apply_unary_op a op) in (result :: tl)
  
  method push_arguments num_args num_locals =
    let arguments = Array.create ~len:(num_args + num_locals) (Numeric 0.0) in
    let rec pop args_left =
      let index = num_args - args_left in 
      if args_left == 0 then () else (
      match op_stack with
      | [] -> raise (What_r_u_doing_lol "Just one operand oh my god it's not that hard.")
      | a :: tl ->
        op_stack <- tl;
        Array.set arguments index a;
        pop (args_left - 1)
      ) in
    pop num_args;
    registers <- arguments

  method interpret_opcodes opcodes = 
    match Stream.peek opcodes, op_stack with
    | None, [] -> ()
    | None, result :: tl -> ()
    | Some op, _ ->
      match op with
      | BinaryOperation op ->
        Stream.junk opcodes;
        op_stack <- (self#binary_op op);
        self#interpret_opcodes opcodes
      | UnaryOperation op ->     
        Stream.junk opcodes;
        op_stack <- (self#unary_op op);
        self#interpret_opcodes opcodes
      | PushSpookyvalue op ->
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
          | [] -> raise (What_r_u_doing_lol "Oh no! A fairy thief stole the only argument you were supposed to pass to the storeLocal op!")
          | a :: tl ->
            Array.set registers op a;
            op_stack <- tl;
            self#interpret_opcodes opcodes
        )
      | Return ->       
        Stream.junk opcodes;
        (match op_stack with
          | [] -> op_stack <- [Void]
          | _ -> ()
        )
      | FunctionDeclaration op ->
        Stream.junk opcodes;      
        self#set_function op;
        self#interpret_opcodes opcodes
      | EndFunctionDeclaration ->
        Stream.junk opcodes;
        self#interpret_opcodes opcodes
      | FunctionCall op ->
        Stream.junk opcodes;      
        let called = Hashtbl.find functions op in
        (match called with
        | None -> raise (What_r_u_doing_lol "can't call a function before you define it")
        | Some called ->
          let old_registers = registers in
          registers <- Array.create ~len:0 (Numeric 0.0);
          self#push_arguments called.num_parameters called.num_locals;
          self#interpret_opcodes (Stream.of_list called.op_codes);
          registers <- old_registers;
          self#interpret_opcodes opcodes      
        )
      | CallBuiltin op ->
        Stream.junk opcodes;
        (match op with
          | 0 -> self#interpreter_scream
          | 1 -> self#creppy_whispers_from_outside
          | _ -> raise (What_r_u_doing_lol "NnnNOOOO an ALIEN BUILTIN! What's it from? What does it do? Too late I already pissed myself.")
        );
        self#interpret_opcodes opcodes
end

let int32_to_char i =
  Char.of_int_exn (Int32.to_int_exn i)

let consume_operand bytes =
  match Stream.peek bytes with
  | None -> raise (What_r_u_doing_lol "not enough operator arguments")
  | Some a ->
    Stream.junk bytes;
    a

let consume_float bytes =
  let top_bits = Int64.shift_left (Int64.of_int32 (consume_operand bytes)) 32 in
  let bottom_bits = Int64.of_int32 (consume_operand bytes) in
  Int64.float_of_bits (Int64.bit_or top_bits bottom_bits)

let consume_string bytes =
  let length = Int32.to_int_exn (consume_operand bytes) in
  let buffer = Buffer.create 100 in
  let rec read_string n =
    match n with
    | 0 -> Buffer.contents buffer
    | n ->
      Buffer.add_char buffer (int32_to_char (consume_operand bytes));
      read_string (n - 1)
  in read_string length

let consume_operand_pair bytes =
  let a = consume_operand bytes in (a, consume_operand bytes)

let rec buffer_opcodes ?b:(buffered=[]) ops =
  match Stream.peek ops with
  | None -> List.rev buffered
  | Some a ->
    Stream.junk ops;
    buffer_opcodes ~b:(a :: buffered) ops

let rec print_opcodes ops =
  match ops with
  | [] -> ()
  | a :: tl -> (
    match a with
    | PushSpookyvalue sp ->
      print_string "SPOOKYVAL PUSH: ";
      print_spookyval sp;
      print_newline();
      print_opcodes tl
    | BinaryOperation sp ->
      print_endline "BINOP";
      print_opcodes tl
    | UnaryOperation sp ->
      print_endline "UNOP";
      print_opcodes tl
    | LoadLocal sp ->
      print_string "LOAD LOCAL: ";    
      print_int sp;
      print_newline();
      print_opcodes tl
    | StoreLocal sp ->
      print_string "STORE LOCAL: ";    
      print_int sp;
      print_newline();
      print_opcodes tl
    | FunctionCall sp ->
      print_string "FUNCTION CALL: ";
      print_int sp;
      print_newline();
      print_opcodes tl
    | CallBuiltin sp ->
      print_string "CALL_BUILTIN: ";
      print_int sp;
      print_newline();
      print_opcodes tl
    | Return ->
      print_endline "RETURN";
      print_opcodes tl      
    | _ -> print_opcodes tl
  )

let rec opcodes bytes =
  let next_opcode i =
    match Stream.peek bytes with
    | None -> None
    | Some num -> (
      match Int32.to_int_exn num with
        | 1 -> Stream.junk bytes; Some (PushSpookyvalue (Numeric (consume_float bytes)))
        | 2 -> Stream.junk bytes; Some (BinaryOperation(Add))
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
          let buffered = buffer_opcodes (opcodes bytes) in     
          let op_codes = optimize_ops buffered in
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
        | 13 -> Stream.junk bytes; Some (PushSpookyvalue (Spookystring (consume_string bytes)))
        | 14 -> Stream.junk bytes; Some (PushSpookyvalue (Void))
        | 15 -> Stream.junk bytes; Some (CallBuiltin (Int32.to_int_exn (consume_operand bytes)))
        | op -> raise (What_r_u_doing_lol (Printf.sprintf "couldn't recognize op: %i%!" op))
    )
  in Stream.from(next_opcode)

let interpret bytestream =
  let vm = new virtual_machine in
  vm#interpret_opcodes (opcodes bytestream)