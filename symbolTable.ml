open Core

(* a simple table *)
type symbol_table = {
  previous: symbol_table option;
  symbols: bool String.Table.t;
}

exception Error of string

let create_spooky_words_regex words =
  let regexp = String.drop_suffix (List.fold_right words ~f:(fun acc curr -> acc ^ "|" ^ curr) ~init:"") 1 in
    Re2.Regex.create_exn regexp

let spooky_words = create_spooky_words_regex [
  "spooky";
  "scary";
  "scream";
  "ghost";
  "skeleton";
  "wolf";
  "jack-o-lantern";
  "bat";
  "dracula";
  "vampire";
  "witch";
  "blood";
  "dead";
  "devil";
  "666";
  "boo";
  "creepy";
]


let not_scary_words = create_spooky_words_regex (* these words arent spooky but code reuse lol *) [
  "not";
  "isnt";
]

let print_table table =
  print_endline "SYMBOL TABLE!";
  Hashtbl.iter_keys table.symbols ~f:(fun a -> print_endline a)

let add_symbol symbol table =
  if Re2.Regex.matches spooky_words symbol then
    if not(Re2.Regex.matches not_scary_words symbol) then
      Hashtbl.set table ~key:symbol ~data:true
    else raise (Error (Printf.sprintf "You thought you could game the system? Scary 👏 variables 👏 only! 👏. Make this scary: %s\n%!" symbol))
  else raise (Error (Printf.sprintf "Look, if you want to program here, you're going to have to write some spooky variable names. Names like: %s just aren't going to cut it.\n%!" symbol))

let populate_symbol_table (ast:Ast.node) =
  let symbols = { previous=None; symbols=String.Table.create(); } in
  let rec visit_ast (curr_node:Ast.node) =
    match curr_node with
    | Ast.FunctionDeclaration syntax ->
      add_symbol syntax.id symbols.symbols;
      List.iter ~f:visit_ast syntax.children
    | Ast.ParamDeclaration syntax ->
      add_symbol syntax symbols.symbols;  
    | Ast.VariableDeclaration syntax ->
      add_symbol syntax.id symbols.symbols;    
      List.iter ~f:visit_ast syntax.children
    | Ast.Program syntax -> List.iter ~f:visit_ast syntax.children  
    | Ast.StatementList syntax -> List.iter ~f:visit_ast syntax.children
    | Ast.ParameterList syntax -> List.iter ~f:visit_ast syntax.children
    | Ast.Statement syntax -> List.iter ~f:visit_ast syntax.children
    | Ast.Reference syntax ->
    if Hashtbl.find symbols.symbols syntax == None then
      raise (Error "Ah! You used a variable before you declared it! I'm so scared!\n")
    | Ast.VariableAssignment syntax ->
    if Hashtbl.find symbols.symbols syntax.id == None then
      raise (Error "And then he... he... He assigned a value to a variable before defining it AHHHHH!\n")
    (* QUESTION: the catchall saves a lot of space, but exhaustiveness would make the code
    more rigorous. Perhaps this is where type refactoring comes into play? at the very least
    the distinction between nonterminals and terminals seems important *)
    | _ -> ()
  in
  visit_ast ast;
  symbols