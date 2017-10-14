open Core

exception Undefined_symbol of string
exception Type_error of string
exception No_main of string

let add_main_call symbol_table opcodes =
  let main_func_index = (
    match SymbolTable.find_symbol "boo!" symbol_table with
    | None -> raise (No_main "you need to have a function called boo! in your program. That's the point of entry. Sorry, but that's the meme.")
    | Some dec -> (
      match dec with 
        | SymbolTable.VariableDeclaration m -> raise (No_main "you have a global variable called 'boo!'? Yeah we need that to be a function.")
        | SymbolTable.FunctionDeclaration m -> m.index
    )
  ) in
  let main_call = [Int32.of_int_exn 10; Int32.of_int_exn main_func_index] in
  List.append opcodes main_call

let push_spookyval spookyval =
  match spookyval with
  | Ast.Numeric syntax ->
    let bits = Int64.bits_of_float syntax in
    let top_bits = Int64.to_int32_exn (Int64.shift_right_logical bits 32) in
    let bottom_bits = Int64.to_int32_exn (Int64.bit_and bits (Int64.shift_right_logical Int64.max_value 32)) in
    [(Int32.of_int_exn 1); top_bits; bottom_bits]
  | Ast.Spookystring syntax ->
    let instruction = [(Int32.of_int_exn 13); (Int32.of_int_exn (String.length syntax))] in
    let contents = List.map (String.to_list_rev syntax) ~f:(fun char -> Int32.of_int_exn (Char.to_int char)) in
    List.append instruction contents
  | Ast.Void -> [(Int32.of_int_exn 14)]

let call_builtin function_name =
  match function_name with
  | "interpreter_scream" -> [Int32.of_int_exn 15; Int32.of_int_exn 0]
  | "creppy_whispers_from_outside" -> [Int32.of_int_exn 15; Int32.of_int_exn 1]
  | _ -> raise (Undefined_symbol "That variable doesn't exist! W-What are you doing? AhhHHHHHHHH!")
  
(* TODO: change hardcoded bytecode numbers to constants *)
let rec compile_ast symbol_table syntax =
    match syntax with
    | Ast.Program syntax -> List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.Spookyval syntax -> push_spookyval syntax
    | Ast.Expression syntax -> List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.Reference syntax ->
        let declaration = SymbolTable.find_symbol syntax symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> [Int32.of_int_exn 6; Int32.of_int_exn declaration]
            | SymbolTable.FunctionDeclaration declaration -> raise (Type_error "call the police they're using a function without calling it in an expression")
        ))
    | Ast.FunctionDeclaration syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> raise (Type_error "this is actually pretty creepy because this should never ever happen but I guess we thought this function was a variable I don't know what to tell you man")
            | SymbolTable.FunctionDeclaration declaration ->
              let declarations = [Int32.of_int_exn 8; Int32.of_int_exn declaration.index; Int32.of_int_exn (Hashtbl.length declaration.parameters.symbols); Int32.of_int_exn (Hashtbl.length declaration.locals.symbols)] in
              let code = compile_ast declaration.locals syntax.code in
              List.append (List.append declarations code) [Int32.of_int_exn 9]
        ))
    | Ast.FunctionCall syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> List.append
          (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
          (call_builtin syntax.id)
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> raise (Type_error "you tried to invoke a regular variable, like a... Like a warlock.")
            | SymbolTable.FunctionDeclaration declaration -> List.append
                (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
                [Int32.of_int_exn 10; Int32.of_int_exn declaration.index]
        ))
    | Ast.ArgumentList syntax -> List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.StatementList syntax -> List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.ParameterList syntax -> List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.VariableDeclaration syntax -> []
    | Ast.ParamDeclaration syntax -> []
    | Ast.VariableAssignment syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> List.append
              (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
              [Int32.of_int_exn 7; Int32.of_int_exn declaration]
            | SymbolTable.FunctionDeclaration declaration -> raise (Type_error "you can't just reassign function bindings, we live in a society")
        ))
    | Ast.ReturnStatement syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [Int32.of_int_exn 11]
    | Ast.Statement syntax -> List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.Operator syntax -> 
      match syntax with
      | Ast.Multiplication syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [Int32.of_int_exn 4]
      | Ast.Addition syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [Int32.of_int_exn 2] 
      | Ast.Division syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [Int32.of_int_exn 5]
      | Ast.Subtraction syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [Int32.of_int_exn 3]
      | Ast.Negation syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int32 list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [Int32.of_int_exn 12]

let compile filename =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    let st = (SymbolTable.populate_symbol_table ast) in
    BytecodeInterpreter.interpret (Stream.of_list (add_main_call st (compile_ast st ast)))
  with
  | Scarerrors.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "%s AAAAAAAAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AA\n%!" (Scarerrors.position filebuf)
  | SymbolTable.Error msg ->
      Printf.eprintf "%s %s\n%!" (Scarerrors.position filebuf) msg  
  | BytecodeInterpreter.What_r_u_doing_lol msg ->
      Printf.eprintf "%s %s\n%!" (Scarerrors.position filebuf) msg
  ;
  close_in input

let spec =
  let open Command.Spec in
  empty
  +> anon("filename" %: file)
  
let command =
  Command.basic
  ~summary:"The Spooky language compiler"
  ~readme:(fun () -> "TODO")
  spec
  (fun filename () -> compile filename)

let () =
  Command.run ~version:"0.10" ~build_info:"RWO" command
