open Core

(* a simple table *)
type symbol_table = { previous: symbol_table option; symbols: declaration String.Table.t; }
and declaration =
| VariableDeclaration of int
| FunctionDeclaration of {
  index: int;
  parameters: symbol_table;
  locals: symbol_table;
}

exception Error of string

let rec print_table ?level:(l=0) table =
  Hashtbl.iter_keys table.symbols ~f:(
    fun a ->
      match Hashtbl.find table.symbols a with
      | None -> ()
      | Some dec -> (
        match dec with
        | FunctionDeclaration dec ->
          print_string (Ast.print_level l);
          print_endline a;
          print_table ~level:(l+1) dec.parameters;
          print_table ~level:(l+1) dec.locals;
        | VariableDeclaration dec ->
          print_string (Ast.print_level l);
          print_endline a;
      )
  )

let rec find_symbol symbol table =
  let search = Hashtbl.find table.symbols symbol in
  if search == None then (
    match table.previous with
    | None -> None
    | Some t -> find_symbol symbol t
  ) else search

let rec add_symbol (symbol:Ast.node) table =
  let symbol_string, declaration = (match symbol with
  | Ast.FunctionDeclaration s ->
    let parameters = populate_symbol_table s.parameters ~s:({
      previous = Some table;
      symbols = String.Table.create();
    }) in (
    s.id,
    FunctionDeclaration({
      index = Hashtbl.length table.symbols;
      parameters;
      locals = populate_symbol_table s.code ~s:({
        previous = Some parameters;
        symbols = String.Table.create();
      });
    }))
  | Ast.VariableDeclaration s -> (s.id, VariableDeclaration(Hashtbl.length table.symbols))
  ) in
  if IsItScary.its_scary symbol_string then
      Hashtbl.set table.symbols ~key:symbol_string ~data:declaration
  else raise (Error (Printf.sprintf "Look, if you want to program here, you're going to have to write some spooky variable names. Names like: %s just aren't going to cut it.\n%!" symbol_string))

and populate_symbol_table ?s:(symbols={
  previous=None;
  symbols=String.Table.create();
}) (ast:Ast.node) : symbol_table =
  let rec visit_ast (curr_node:Ast.node) =
    match curr_node with
    | Ast.FunctionDeclaration syntax -> add_symbol curr_node symbols
    | Ast.VariableDeclaration syntax -> add_symbol curr_node symbols
    | Ast.Program syntax -> List.iter ~f:visit_ast syntax.children  
    | Ast.StatementList syntax -> List.iter ~f:visit_ast syntax.children
    | Ast.ParameterList syntax -> List.iter ~f:visit_ast syntax.children
    | Ast.Statement syntax -> List.iter ~f:visit_ast syntax.children
    | Ast.Reference syntax ->
    if find_symbol syntax symbols == None then
      raise (Error "Ah! You used a variable before you declared it! I'm so scared!\n")
    | Ast.VariableAssignment syntax ->
    if find_symbol syntax.id symbols == None then
      raise (Error "And then he... he... He assigned a value to a variable before defining it AHHHHH!\n")
    (* QUESTION: the catchall saves a lot of space, but exhaustiveness would make the code
    more rigorous. Perhaps this is where type refactoring comes into play? at the very least
    the distinction between nonterminals and terminals seems important *)
    | _ -> ()
  in
  visit_ast ast;
  symbols