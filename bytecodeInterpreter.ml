open Core

exception What_r_u_doing_lol of string

type binary_operation =
  | Add
  | DivideNumeric
  | MultiplyNumeric
  | SubtractNumeric
  | Equal
  | Greater
  | Less
  | Lequal
  | Gequal
  | Nequal

type spookyval =
  | Numeric of float
  | Spookystring of string
  | Void
  | Booolean of bool
  | Array of spookyval array
  | Object of spookyval String.Table.t

type unary_operation =
  | Negation
  | Not

type opcode =
  | PushSpookyvalue of spookyval
  | BinaryOperation of binary_operation
  | LoadLocal of int
  | StoreLocal of int
  | LoadGlobal of int
  | StoreGlobal of int
  | UnaryOperation of unary_operation
  | FunctionDeclaration of function_declaration
  | FunctionCall of int
  | CallBuiltin of int
  | IfDefinition of if_definition
  | IfElseDefinition of if_else_definition
  | LoopDefinition of loop_definition
  | Accessor of accessor
  | ArrayLiteralDefinition of array_literal_definition
  | ObjectLiteralDefinition of opcode list
  | Assignment of key_val
  | AssignmentStatement of assignment_statement
  | AssignmentAccessor of opcode list
  | Return
and function_declaration = {
  symbol: int;
  num_parameters: int;
  num_locals: int;
  op_codes: opcode list;
}
and if_definition = {
  test_code: opcode list;
  execution_code: opcode list;
}
and if_else_definition = {
  test_code: opcode list;
  true_code: opcode list;
  false_code: opcode list;
}
and loop_definition = {
  test_code: opcode list;
  loop_code: opcode list;
}
and accessor = {
  store_code: opcode list;
  key_code: opcode list;
}
and key_val = (opcode list * opcode list)
and array_literal_definition = {
  length: int;
  assignments: opcode list;
}
and assignment_statement = {
  id: int;
  accessors: opcode list;
  key: opcode list;
  value: opcode list;
}

let spooky_to_bool spval =
  match spval with
  | Numeric n -> not(n =. 0.0)
  | Spookystring s -> not(String.equal s "")
  | Void -> false
  | Booolean b -> b
  | Array a -> not(Array.is_empty a)
  | Object o -> true

let rec spooky_to_string spval =
  match spval with
  | Numeric n -> string_of_float n
  | Spookystring s -> s
  | Void -> "Void"
  | Booolean b -> if b then "True" else "False"
  | Array a ->
    "🍫\n" ^
    (Array.fold_right a ~init:"" ~f:(fun spval acc -> (spooky_to_string spval) ^ "🍬\n" ^ acc)) ^
    "🍭\n"
  | Object o ->
    "🍫\n" ^ Hashtbl.fold o ~init:"" ~f:(
      fun ~key:k ~data:spval acc ->
        acc ^ k ^ " 😱 " ^ (spooky_to_string spval) ^ "🍬\n"
    ) ^ "🍭\n"

let spooky_equality a b =
  match a, b with
  | Numeric a, Numeric b -> a =. b
  | Spookystring a, Spookystring b -> String.equal a b
  | Void, Void -> true
  | Booolean a, Booolean b -> phys_equal a b
  | _, _ -> false

let spooky_greater a b =
  match a, b with
  | Numeric a, Numeric b -> a > b
  | Spookystring a, Spookystring b -> a > b
  | Void, Void -> false
  | Booolean a, Booolean b -> false
  | _, _ -> false

let spooky_less a b =
  match a, b with
  | Numeric a, Numeric b -> a < b
  | Spookystring a, Spookystring b -> a < b
  | Void, Void -> false
  | Booolean a, Booolean b -> false
  | _, _ -> false

let print_spookyval spval =
  let str_spval = spooky_to_string spval in
  if IsItScary.its_scary str_spval then (
    print_string str_spval;
    print_string " "
  ) else
    raise (What_r_u_doing_lol (Printf.sprintf "Our language's entire raison d'etre is based around being spooky. In the 'spirit' of that, we only allow scary IO.\n Make this scary and then we'll talk: %s%!" str_spval))

let debug_spookyval spval =
  match spval with
    | Numeric n ->
      print_string "NUMERIC: ";
      print_endline (spooky_to_string spval)   
    | Spookystring s ->
      print_string "SPOOKYSTRING: ";
      print_endline (spooky_to_string spval)
    | Void ->
      print_endline (spooky_to_string spval)    
    | Booolean b ->
      print_string "BOOLEAN: ";
      print_endline (spooky_to_string spval)      
    | Object o ->
      print_string "OBJECT: ";
      print_endline(spooky_to_string spval)
    | Array arr ->
      print_string "ARRAY: ";
      print_endline (spooky_to_string spval)

let rec debug_opcodes ops =
  match ops with
  | [] -> ()
  | a :: tl -> (
    match a with
    | PushSpookyvalue sp ->
      print_string "SPOOKYVAL PUSH: ";
      debug_spookyval sp;
      print_newline();
      debug_opcodes tl
    | BinaryOperation sp ->
      print_endline "BINOP";
      debug_opcodes tl
    | UnaryOperation sp ->
      print_endline "UNOP";
      debug_opcodes tl
    | LoadLocal sp ->
      print_string "LOAD LOCAL: ";    
      print_int sp;
      print_newline();
      debug_opcodes tl
    | StoreLocal sp ->
      print_string "STORE LOCAL: ";    
      print_int sp;
      print_newline();
      debug_opcodes tl
    | LoadGlobal sp ->
      print_string "LOAD GLOBAL: ";    
      print_int sp;
      print_newline();
      debug_opcodes tl
    | StoreGlobal sp ->
      print_string "STORE GLOBAL: ";    
      print_int sp;
      print_newline();
      debug_opcodes tl
    | FunctionCall sp ->
      print_string "FUNCTION CALL: ";
      print_int sp;
      print_newline();
      debug_opcodes tl
    | CallBuiltin sp ->
      print_string "CALL_BUILTIN: ";
      print_int sp;
      print_newline();
      debug_opcodes tl
    | Accessor sp ->
      print_string "ACCESSOR: ";
      print_newline();
      debug_opcodes tl
    | Return ->
      print_endline "RETURN";
      debug_opcodes tl
    | Assignment a ->
      print_endline "ASSIGNMENT";
      debug_opcodes tl
    | AssignmentStatement _ ->
      print_endline "ASSIGNMENT_STATEMENT: ";
      debug_opcodes tl
    | AssignmentAccessor ass_acc (* ha ha ha ha ha *) ->
      print_endline "ASSIGNMENT_ACCESSOR: ";
      debug_opcodes tl
    | _ ->
      print_endline "WEEEE!";
      debug_opcodes tl
  )

let debug_opcode_object opcode =
  match opcode with
  | LoopDefinition op ->
    print_endline "LOOP DEFINITION: ";
    print_endline "TEST_CODE: ";
    debug_opcodes op.test_code;
    print_endline "LOOP_CODE: ";
    debug_opcodes op.loop_code;
    print_newline()
  | op_obj -> debug_opcodes [op_obj]

let apply_unary_op a op =
  match op with
  | Negation ->
    (match a with
      | Numeric anum -> Numeric(~-. anum)
      | _ -> Void
    )
  | Not -> Booolean(not (spooky_to_bool a))

let apply_binary_op a b op =
  match op with
  | Add ->
    (match a, b with
    | Numeric a, Numeric b -> Numeric(a +. b)
    | _, Spookystring bsp -> Spookystring (spooky_to_string a ^ bsp)
    | Spookystring bsp, _ -> Spookystring (bsp ^ spooky_to_string b)
    | spval, Array arr -> (
      match spval with
      | Array arrb -> Array (Array.append arrb arr)
      | _ -> Array (Array.append (Array.of_list [spval]) arr)
    )
    | Array arr, spval -> (
      match spval with
      | Array arrb -> Array (Array.append arr arrb)
      | _ -> Array (Array.append arr (Array.of_list [spval]))
    )
    | _, _ -> Void)
  | DivideNumeric ->
    (match a, b with
    | Numeric a, Numeric b -> Numeric(a /. b)
    | _, _ -> Void)
  | MultiplyNumeric ->
    (match a, b with
    | Numeric a, Numeric b -> Numeric(a *. b)
    | _, _ -> Void)
  | SubtractNumeric ->
    (match a, b with
    | Numeric a, Numeric b -> Numeric(a -. b)
    | _, _ -> Void)
  | Equal -> Booolean(spooky_equality a b)
  | Greater -> Booolean(spooky_greater a b)
  | Less -> Booolean(spooky_less a b)
  | Lequal -> Booolean(spooky_greater a b)
  | Gequal -> Booolean(not (spooky_less a b))
  | Nequal -> Booolean(not (spooky_equality a b))

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
    | b :: a :: tl, op -> (
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
  val mutable globals = Int.Table.create()
  val mutable registers = ((Array.create ~len:0 (Void)): spookyval array)
  val mutable op_stack = ([] : spookyval list)
  val mutable debug = false
  val mutable context = Void

  method enable_debug =
    debug <- true

  method print_and_then_scream =
    match op_stack with
      | [] -> print_endline "AHHHHHHHHHHHH!"
      | a :: tl ->
        print_spookyval a;
        print_endline "AHHHHHHHHHHHH!";
        op_stack <- tl

  method spooky_input =
    let very_creppy = read_line() in
      if IsItScary.its_scary very_creppy then
        op_stack <- (Spookystring(very_creppy) :: op_stack)
      else
        raise (What_r_u_doing_lol "Dear user, the handsome genius behind this program wrote it in the world's first spooky-complete language, a computational model that only accepts spooky I/O.\nYou attempted to input something 'unspooky', and we cannot allow that under our invariants. So the program crashed. Also, there's a skeleton behind you! AHHHHH!")

  method scary_length =
    let top = self#get_op_top in
    op_stack <- (
    match top with
    | Spookystring sp -> Numeric(Int.to_float (String.length sp))
    | Array arr -> Numeric(Int.to_float (Array.length arr))
    | _ -> Void
    ) :: op_stack
  
  method skeleton_keys =
    let top = self#get_op_top in
    op_stack <- (
    match top with
    | Object o -> Array ( Array.of_list (List.map (Hashtbl.keys o) ~f:(fun a -> Spookystring a)))
    | _ -> Void
    ) :: op_stack

  method debug_op_stack =
    print_string "op stack = ";
    List.iter ~f:(fun a -> debug_spookyval a; print_string " , ") op_stack;
    print_newline()

  method debug_registers =
    print_string "registers = ";
    Array.iter ~f:(fun a -> debug_spookyval a; print_string " , ") registers;
    print_newline()

  method set_function function_dec =
    Hashtbl.set functions ~key:function_dec.symbol ~data:function_dec

  method set_global global_dec spookyval =
    Hashtbl.set globals ~key:global_dec ~data:spookyval

  method get_global global_dec =
    match Hashtbl.find globals global_dec with
      | None -> Void
      | Some spookyval -> spookyval

  method binary_op op =
    match op_stack with
    | [] -> raise (What_r_u_doing_lol "There weren't enough operands for this operation. How is this possible when our parser demands operator satisfaction? Who knows. Still seems like your fault. Program crashing because 2 spooky, you know the meme.")
    | one_op :: [] -> raise (What_r_u_doing_lol "There weren't enough operands for this operation. How is this possible when our parser demands operator satisfaction? Who knows. Still seems like your fault. Program crashing because 2 spooky, you know the meme.")
    | b :: a :: tl -> let result = (apply_binary_op a b op) in (result :: tl)
  
  method unary_op op =
    match op_stack with
    | [] -> raise (What_r_u_doing_lol "There weren't enough operands for this... unary op. You just needed one man, come on. Gotta do the meme: So scared! Crashing the program!")
    | a :: tl -> let result = (apply_unary_op a op) in (result :: tl)

  method get_op_top =
    match op_stack with
    | [] -> Void
    | a :: tl ->
      op_stack <- tl;
      a
  
  method test_op_stack test =
    match op_stack with
    | [] -> test Void
    | a :: tl ->
      op_stack <- tl;
      test a

  method eval expr =
    self#interpret_opcodes (Stream.of_list expr);
    self#get_op_top

  method set_context obj =
    context <- obj

  method assign_to_context key spval =
    if debug then (
      print_endline "ASSIGNING TO CONTEXT!";
      print_string "CONTEXT: ";
      debug_spookyval context;
      print_string "KEY: ";
      debug_spookyval key;
      print_newline();
      print_string "VAL: ";
      debug_spookyval spval;
      print_newline()
    );
    match context with
    | Array arr -> ( match key with
      | Numeric key ->
        let index = Int.of_float key in
        Array.set arr index spval;
      | _ -> ())
    | Object o ->
      let str_key = spooky_to_string key in
      Hashtbl.set o ~key:str_key ~data:spval
    | _ -> ()

  
  method push_arguments num_args num_locals =
    let arguments = Array.create ~len:(num_args + num_locals) (Void) in
    let rec pop args_left =
      let index = args_left in 
      if args_left == -1 then () else (
      match op_stack with
      | [] -> raise (What_r_u_doing_lol "Just one operand oh my god it's not that hard.")
      | a :: tl ->
        op_stack <- tl;
        Array.set arguments index a;
        pop (args_left - 1)
      ) in
    pop (num_args - 1);
    registers <- arguments

  method access_context key = (
    if debug then (
      print_endline "ACCESSING CONTEXT!";
      print_string "CONTEXT: ";
      debug_spookyval context;
      print_newline();
      print_string "KEY: ";
      debug_spookyval key;
      print_newline()
    );
    match context with
    | Spookystring sp ->
      (match key with
      | Numeric n ->
        let index = Int.of_float n in
        let cr = String.get sp index in
        Spookystring(String.of_char cr)
      | _ -> Void)
    | Array arr ->
      (match key with
      | Numeric n ->
        let index = Int.of_float n in
        Array.get arr index
      | _ -> Void)
    | Object obj ->
      let str_key = spooky_to_string key in
      (match Hashtbl.find obj str_key with
      | Some spval -> spval
      | None -> Void)
    | _ -> Void
  )
  
  method try_loop (loop_statement:loop_definition) =
    if debug then (
      print_endline "LOOP ITERATION";
      self#debug_registers;
      self#debug_op_stack
    );
    self#interpret_opcodes (Stream.of_list loop_statement.test_code);
    self#test_op_stack (fun a ->
      if debug then (
        print_endline "AFTER LOOP TEST";
        self#debug_registers;
        self#debug_op_stack;
        print_string "TEST RESULT: ";
        debug_spookyval a;
        print_newline()
      );
      match a with
      | Void -> ()
      | Booolean spookyval -> (
        if spookyval then (
          self#interpret_opcodes (Stream.of_list loop_statement.loop_code);
          self#try_loop loop_statement
        )
        else ()
      )
      | Numeric spookyval ->
        if spookyval =. 0.0 then ()
        else
          self#interpret_opcodes (Stream.of_list loop_statement.loop_code);
          self#try_loop loop_statement    
      | _ ->
        self#interpret_opcodes (Stream.of_list loop_statement.loop_code);
        self#try_loop loop_statement        
    );

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
          | [] -> raise (What_r_u_doing_lol "Oh no! A skeleton thief stole the only argument you were supposed to pass to the storeLocal op! I'm joking. You made a mistake. Mistakes scare me! That makes programs crash! Boom.")
          | a :: tl ->
            Array.set registers op a;
            op_stack <- tl;
            self#interpret_opcodes opcodes
        )
      | LoadGlobal op ->     
        Stream.junk opcodes;
        op_stack <- (self#get_global op) :: op_stack;
        self#interpret_opcodes opcodes
      | StoreGlobal op ->
        Stream.junk opcodes;
        (match op_stack with
          | [] -> raise (What_r_u_doing_lol "Oh no! A skeleton thief stole the only argument you were supposed to pass to the storeGlobal op! I'm joking. You made a mistake. Mistakes scare me! That makes programs crash! Boom.")
          | a :: tl ->
            self#set_global op a;
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
      | Accessor accessor ->
        Stream.junk opcodes;
        self#interpret_opcodes (Stream.of_list accessor.store_code);
        self#test_op_stack (fun store ->
          let old_context = context in
          self#set_context store;
          self#interpret_opcodes (Stream.of_list accessor.key_code);
          let key = self#get_op_top in
          op_stack <- (self#access_context key) :: op_stack;
          self#set_context old_context
        );
        self#interpret_opcodes opcodes
      | IfDefinition if_statement ->
        Stream.junk opcodes;
        self#interpret_opcodes (Stream.of_list if_statement.test_code);
        self#test_op_stack (fun a ->
          match a with
          | Void -> ()
          | Booolean spookyval ->
            if spookyval then self#interpret_opcodes (Stream.of_list if_statement.execution_code) else ()
          | Numeric spookyval ->
            if spookyval =. 0.0 then () else self#interpret_opcodes (Stream.of_list if_statement.execution_code)
          | _ -> self#interpret_opcodes (Stream.of_list if_statement.execution_code)
        );
        self#interpret_opcodes opcodes
      | IfElseDefinition if_else_statement ->
        Stream.junk opcodes;   
        self#interpret_opcodes (Stream.of_list if_else_statement.test_code);
        self#test_op_stack (fun a ->
          match a with
          | Void -> self#interpret_opcodes (Stream.of_list if_else_statement.false_code)
          | Booolean spookyval ->
            if spookyval then self#interpret_opcodes (Stream.of_list if_else_statement.true_code)
            else self#interpret_opcodes (Stream.of_list if_else_statement.false_code)
          | Numeric spookyval ->
            if spookyval =. 0.0 then self#interpret_opcodes (Stream.of_list if_else_statement.false_code)
            else self#interpret_opcodes (Stream.of_list if_else_statement.true_code)
          | _ -> self#interpret_opcodes (Stream.of_list if_else_statement.true_code)
        );
        self#interpret_opcodes opcodes
      | LoopDefinition loop_statement ->
        Stream.junk opcodes;      
        self#try_loop loop_statement;
        self#interpret_opcodes opcodes
      | ArrayLiteralDefinition array_literal_definition ->
        Stream.junk opcodes;
        let len = array_literal_definition.length in
        let arr = Array(Array.create ~len:len Void) in
        let old_context = context in
        self#set_context arr;
        op_stack <- (arr :: op_stack);
        self#interpret_opcodes (Stream.of_list array_literal_definition.assignments);
        self#set_context old_context;
        self#interpret_opcodes opcodes        
      | ObjectLiteralDefinition object_literal_definition ->
        Stream.junk opcodes;      
        let obj = Object(String.Table.create()) in
        let old_context = context in        
        self#set_context obj;
        op_stack <- (obj :: op_stack);
        self#interpret_opcodes (Stream.of_list object_literal_definition);
        self#set_context old_context;        
        self#interpret_opcodes opcodes        
      | Assignment a ->
        Stream.junk opcodes;
        let key_expr, val_expr = a in
        let key = self#eval key_expr in
        let spval = self#eval val_expr in
        self#assign_to_context key spval;
        self#interpret_opcodes opcodes   
      | AssignmentStatement assign_statement ->
        Stream.junk opcodes;
        let base_id = assign_statement.id in
        let base_obj = (Array.get registers base_id) in
        self#set_context base_obj;
        self#interpret_opcodes (Stream.of_list assign_statement.accessors);
        self#interpret_opcodes (Stream.of_list assign_statement.key);
        let key = self#get_op_top in
        self#interpret_opcodes (Stream.of_list assign_statement.value);
        let spval = self#get_op_top in
        self#assign_to_context key spval;
        self#interpret_opcodes opcodes    
      | AssignmentAccessor ass_acc (* lmaoooo *) ->
        Stream.junk opcodes;      
        self#interpret_opcodes (Stream.of_list ass_acc);
        let key = self#get_op_top in
        let new_obj = self#access_context key in 
        self#set_context new_obj;
        self#interpret_opcodes opcodes        
      | FunctionCall op ->
        Stream.junk opcodes;      
        let called = Hashtbl.find functions op in
        (match called with
        | None -> raise (What_r_u_doing_lol "Eeeeee a program that called a function before it defined it! Guess I'm fear-crashing!")
        | Some called ->
          let old_registers = registers in
          registers <- Array.create ~len:0 Void;
          self#push_arguments called.num_parameters called.num_locals;
          self#interpret_opcodes (Stream.of_list called.op_codes);
          registers <- old_registers;
          self#interpret_opcodes opcodes      
        )
      | CallBuiltin op ->
        Stream.junk opcodes;
        (match op with
          | 0 -> self#print_and_then_scream
          | 1 -> self#spooky_input
          | 2 -> self#scary_length
          | 3 -> self#skeleton_keys
          | _ -> raise (What_r_u_doing_lol "NnnNOOOO a MYSTERIOUS BUILTIN! What's it from? What does it do? Too late I already peed my pants.")
        );
        self#interpret_opcodes opcodes
end

let int32_to_char i =
  Char.of_int_exn (Int32.to_int_exn i)

let consume_operand bytes =
  match Stream.peek bytes with
  | None -> raise (What_r_u_doing_lol "Your bytestream ended early.. the suspense is terrifying! We're just going to crash the program.")
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

let rec buffer_assignments ?b:(buffered=[]) ops =
  match Stream.peek ops with
  | None -> List.rev buffered
  | Some a ->
    match a with
    | Assignment kv ->
      Stream.junk ops;
      buffer_assignments ~b:(a :: buffered) ops
    | _ -> List.rev buffered

let rec buffer_assignment_accessors ?b:(buffered=[]) ops =
  match Stream.peek ops with
  | None -> List.rev buffered
  | Some a ->
    match a with
    | AssignmentAccessor ass_acc (* lol *) ->
      Stream.junk ops;
      buffer_assignment_accessors ~b:(a :: buffered) ops
    | _ -> List.rev buffered

let rec opcodes ?d:(debug=false) bytes =
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
          let buffered = buffer_opcodes (opcodes ~d:debug bytes) in     
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
        | 16 -> Stream.junk bytes; Some (LoadGlobal (Int32.to_int_exn (consume_operand bytes)))
        | 17 -> Stream.junk bytes; Some (StoreGlobal (Int32.to_int_exn (consume_operand bytes)))
        | 18 -> Stream.junk bytes; Some (PushSpookyvalue (Booolean(true)))
        | 19 -> Stream.junk bytes; Some (PushSpookyvalue (Booolean(false)))
        | 20 -> Stream.junk bytes; Some (BinaryOperation(Equal))
        | 21 -> Stream.junk bytes; Some (BinaryOperation(Less))
        | 22 -> Stream.junk bytes; Some (BinaryOperation(Greater))
        | 23 -> Stream.junk bytes; Some (BinaryOperation(Gequal))
        | 24 -> Stream.junk bytes; Some (BinaryOperation(Lequal))
        | 25 ->
          Stream.junk bytes;
          let test_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let test_code = optimize_ops test_buffered in
          let execution_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let execution_code = optimize_ops execution_buffered in
          Some (IfDefinition {
            test_code;
            execution_code;
          })
        | 26 ->
          Stream.junk bytes;
          let test_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let test_code = optimize_ops test_buffered in
          let true_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let true_code = optimize_ops true_buffered in
          let false_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let false_code = optimize_ops false_buffered in
          Some (IfElseDefinition {
            test_code;
            true_code;
            false_code;
          })
        | 27 ->
          Stream.junk bytes;
          let test_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let test_code = optimize_ops test_buffered in
          let loop_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let loop_code = optimize_ops loop_buffered in
          let loop_def = LoopDefinition {
            test_code;
            loop_code;
          } in
          if debug then debug_opcode_object loop_def;
          Some loop_def
        | 28 -> Stream.junk bytes; Some (BinaryOperation(Nequal));
        | 29 -> Stream.junk bytes; Some (UnaryOperation(Not))
        | 30 ->
          Stream.junk bytes;
          let store_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let store_code = optimize_ops store_buffered in
          let key_buffered = buffer_opcodes (opcodes ~d:debug bytes) in
          let key_code = optimize_ops key_buffered in
          Some (Accessor {
            store_code;
            key_code;
          })
        | 31 ->
          Stream.junk bytes;
          let length = Int32.to_int_exn (consume_operand bytes) in
          let assignment_codes = opcodes ~d:debug bytes in
          let assignments = buffer_assignments assignment_codes in
          Some (ArrayLiteralDefinition {
            length;
            assignments;
          })
        | 32 ->
          Stream.junk bytes;
          let assignments = buffer_assignments (opcodes ~d:debug bytes) in
          Some (ObjectLiteralDefinition assignments)
        | 33 ->
          Stream.junk bytes;
          let key = optimize_ops (buffer_opcodes (opcodes ~d:debug bytes)) in
          let spvalue = optimize_ops (buffer_opcodes (opcodes ~d:debug bytes)) in
          Some (Assignment (key, spvalue))
        | 34 ->
          Stream.junk bytes;
          let id = Int32.to_int_exn (consume_operand bytes) in
          let opcode_stream = opcodes ~d:debug bytes in
          let accessors = optimize_ops (buffer_assignment_accessors opcode_stream) in
          let key = optimize_ops (buffer_opcodes opcode_stream) in       
          let value = optimize_ops (buffer_opcodes (opcodes ~d:debug bytes)) in
          Some (AssignmentStatement {
            id;
            accessors;
            key;
            value;
          })
        | 35 ->
          Stream.junk bytes;
          let accessor_ops = optimize_ops (buffer_opcodes (opcodes ~d:debug bytes)) in      
          Some (AssignmentAccessor accessor_ops)
        | op -> raise (What_r_u_doing_lol (Printf.sprintf "An alien opcode from outer space: %i .We ran away from the execution of your program in fear!%!" op))
    )
  in Stream.from(next_opcode)

let interpret ?d:(debug=false) bytestream =
  if debug then (
    print_endline "DEBUGGING BYTECODE: ";
    print_newline()
  );
  let vm = new virtual_machine in
  if debug then vm#enable_debug;
  vm#interpret_opcodes (opcodes ~d:debug bytestream)