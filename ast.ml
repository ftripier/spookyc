open Core

(* TODO: someday, when I'm better at algebraic data types, figure out how
to consolidate these duplicate fields/variants *)
(* TODO: not all of these nodes need a children field. Rename the edges for
nonterminal nodes to something more semantically informative. *)
(* TODO: perhaps these types could be factored into nonterminal nodes, and
terminal nodes *)
type node =
  | Program of { children: node list; }
  | Numeric of float
  | Reference of string  
  | Expression of { children: node list; }
  | FunctionDeclaration of { id:string; parameters: node; code: node; }
  | FunctionCall of { id:string; children: node list; }
  | ArgumentList of { children: node list; }  
  | StatementList of { children: node list; }
  | ParameterList of { children: node list; }
  | ParamDeclaration of string  
  | VariableDeclaration of { id: string; children: node list; }
  | VariableAssignment of { id: string; children: node list; }
  | ReturnStatement of { children: node list; }
  | Statement of { children: node list; }
  | Operator of operator
and operator = 
  | Multiplication of { children: node list; }
  | Addition of { children: node list; }
  | Division of { children: node list; }
  | Subtraction of { children: node list; }
  | Negation of { children: node list; }

let serialize_operator n =
  match n with
  | Multiplication n -> "Multiplication!\n"
  | Addition n -> "Addition!\n"
  | Division n -> "Division!\n"
  | Subtraction n -> "Subtraction!\n"
  | Negation n -> "Negation!\n"

let serialize_node (n: node) =
  match n with
    | Program n -> "Program!\n"  
    | Numeric n -> Printf.sprintf "Numeric!: %f\n%!" n
    | Expression n -> "Expression!\n"
    | Reference n -> Printf.sprintf "Reference!: %s\n%!" n
    | Statement n -> "Statement!\n"
    | ReturnStatement n -> "ReturnStatement!\n"    
    | FunctionDeclaration n -> Printf.sprintf "FunctionDeclaration!: %s\n%!" n.id
    | ParamDeclaration n -> Printf.sprintf "ParamDeclaration!: %s\n%!" n   
    | FunctionCall n -> Printf.sprintf "FunctionCall!: %s\n%!" n.id
    | ArgumentList n -> "ArgumentList!\n"    
    | StatementList n -> "StatementList!\n"
    | ParameterList n -> "ParameterList!\n"
    | VariableDeclaration n -> Printf.sprintf "VariableDeclaration!: %s\n%!" n.id
    | VariableAssignment n -> "VariableAssignment!\n"
    | Operator n -> serialize_operator n

(* TODO: make tail-call recursive *)
let rec print_level l = 
  match l with
  | 0 -> ""
  | _ -> "    " ^ print_level (l - 1)

let rec print_ast (syntax:node) ?level:(l=0) =
  print_string (print_level l);
  print_endline (serialize_node syntax);
  match syntax with
  | Program syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children  
  | Numeric syntax -> ()
  | Expression syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | Reference syntax -> ()
  | FunctionDeclaration syntax ->
    print_ast ~level:(l + 1) syntax.parameters;
    print_ast ~level:(l + 1) syntax.code
  | FunctionCall syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | ArgumentList syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children  
  | StatementList syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | ParameterList syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | VariableDeclaration syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | ParamDeclaration syntax -> ()  
  | VariableAssignment syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | ReturnStatement syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | Statement syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | Operator syntax -> 
    match syntax with
    | Multiplication syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children 
    | Addition syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children 
    | Division syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children 
    | Subtraction syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
    | Negation syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children 


  
