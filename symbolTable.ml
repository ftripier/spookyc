open Core

(* a simple table *)
type symbol_table = {
  previous: symbol_table option;
  symbols: bool String.Table.t;
}

let print_table table =
  print_endline "SYMBOL TABLE!";
  Hashtbl.iter_keys table.symbols ~f:(fun a -> print_endline a)

let add_symbol symbol table =
  Hashtbl.set table ~key:symbol ~data:true

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
    (* QUESTION: the catchall saves a lot of space, but exhaustiveness would make the code
    more rigorous. Perhaps this is where type refactoring comes into play? at the very least
    the distinction between nonterminals and terminals seems important *)
    | _ -> ()
  in
  visit_ast ast;
  symbols