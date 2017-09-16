open Core.Std

(* TODO: someday, when I'm better at algebraic data types, figure out how
to consolidate these duplicate fields/variants *)
(* TODO: not all of these nodes need a children field. Rename the edges for
nonterminal nodes to something more semantically informative. *)
type node =
  | Program of { children: node list; }
  | Numeric of int
  | Reference of string  
  | Expression of { children: node list; }
  | FunctionDeclaration of { id:string; children: node list; }
  | StatementList of { children: node list; }
  | ParameterList of { children: node list; }  
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
    | Numeric n -> Printf.sprintf "Numeric!: %d\n%!" n
    | Expression n -> "Expression!\n"
    | Reference n -> Printf.sprintf "Reference!: %s\n%!" n
    | Statement n -> "Statement!\n"
    | ReturnStatement n -> "ReturnStatement!\n"    
    | FunctionDeclaration n -> "FunctionDeclaration!\n"
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
  | FunctionDeclaration syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | StatementList syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
  | ParameterList syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children  
  | VariableDeclaration syntax -> List.iter ~f:(print_ast ~level:(l + 1)) syntax.children
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


  
