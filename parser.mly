%token <float> NUMBER
%token PLUS MINUS TIMES DIV
%token LESS GREATER GEQUAL LEQUAL EQUAL NEQUAL
%token NOT
%token START_ACCESSOR END_ACCESSOR
%token COLLECTION_LITERAL_LEFT COLLECTION_LITERAL_RIGHT
%token IF ELSE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON
%token EOF
%token FUNC
%token VOID
%token TRUE
%token FALSE
%token VAR_DEC
%token LOOP
%token <string> STR
%token RETURN
%token <string> ID
%token ASSIGN

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left LESS GREATER GEQUAL LEQUAL EQUAL NEQUAL /* a little higher precedence */
%nonassoc NOT UMINUS    /* highest precedence */

%start <Ast.node> main
%{

  open Ast

%}
%%

main: statements = declarations { Ast.Program { children = statements } }

declarations:
| dec = EOF { [] }
| dec = function_declaration decs = declarations { dec :: decs }
| dec = variable_declaration decs = declarations { dec :: decs }

function_declaration: FUNC id = ID LPAREN plist = parameter_list LBRACE statements = statementseq {
    Ast.FunctionDeclaration {
        id;
        code = Ast.StatementList { children = statements; };
        parameters = Ast.ParameterList { children = plist; };
    }
}

statementseq:
| RBRACE { [] }
| stmt = statement RBRACE { [stmt] }
| stmt = statement m = statementseq {  stmt :: m }

statement:
| stmt = variable_declaration { stmt }
| stmt = variable_assignment { stmt }
| stmt = return_statement { stmt }
| stmt = void_expr { stmt }
| stmt = conditional { stmt }
| stmt = loop { stmt }

variable_declaration: VAR_DEC id = ID SEMICOLON { Ast.VariableDeclaration { id; children=[]; } }
variable_assignment: id = ID ASSIGN e = expr SEMICOLON { Ast.VariableAssignment { id; children = [e]; } }
return_statement: RETURN ret = expr SEMICOLON { Ast.ReturnStatement { children = [ret]; } }
void_expr: e = expr SEMICOLON { Ast.Expression { children = [e]; } }

loop: LOOP LPAREN e = expr RPAREN LBRACE statements = statementseq
    { Ast.LoopStatement {test = e; statements; } }

conditional:
| IF LPAREN e = expr RPAREN LBRACE statements = statementseq
    { Ast.IfStatement { test = e; statements; } }
| IF LPAREN e = expr RPAREN LBRACE if_statements = statementseq ELSE LBRACE else_statements = statementseq
    { Ast.IfElseStatement { test = e; if_statements; else_statements; } }

parameter_list:
| RPAREN { [] }
| param = ID RPAREN { [ Ast.ParamDeclaration param ] }
| param = ID COMMA p = parameter_list {  (Ast.ParamDeclaration param) :: p }

argument_list:
| RPAREN { [] }
| param = expr RPAREN { [param] }
| param = expr COMMA p = argument_list {  param :: p }

array_list:
| COLLECTION_LITERAL_RIGHT { [] }
| e = expr COLLECTION_LITERAL_RIGHT { [e] }
| e = expr RETURN COLLECTION_LITERAL_RIGHT { [e] }
| e = expr RETURN es = array_list { e :: es }

key_value_list:
| COLLECTION_LITERAL_LEFT { [] }
| key = STR ASSIGN e = expr COLLECTION_LITERAL_RIGHT { [(key, e)] }
| key = STR ASSIGN e = expr RETURN COLLECTION_LITERAL_RIGHT { [(key, e)] }
| key = STR ASSIGN e = expr RETURN kvlist = key_value_list { (key, e) :: kvlist }

expr:
| v = VOID
    { Ast.Spookyval(Ast.Void) }
| i = NUMBER
    { Ast.Spookyval(Ast.Numeric i) }
| s = STR
    { Ast.Spookyval(Ast.Spookystring s) }
| t = TRUE
    { Ast.Spookyval(Ast.True) }
| f = FALSE
    { Ast.Spookyval(Ast.False) }
| COLLECTION_LITERAL_LEFT exp = array_list
    { Ast.Spookyval(Ast.Array(exp)) }
| COLLECTION_LITERAL_LEFT exp = key_value_list
    { Ast.Spookyval(Ast.Object(exp)) }
| id = ID
    { Ast.Reference id }
| id = ID LPAREN args = argument_list {
    Ast.FunctionCall {
        id;
        children=[
            Ast.ArgumentList { children = args; };
        ];
    }
}
| LPAREN e = expr RPAREN
    {  Ast.Expression { children = [e]; } }
| e1 = expr PLUS e2 = expr
    {  Ast.Operator(Ast.Addition { children = [e1; e2]; }) }
| e1 = expr MINUS e2 = expr
    {  Ast.Operator(Ast.Subtraction { children = [e1; e2]; }) }
| e1 = expr TIMES e2 = expr
    {  Ast.Operator(Ast.Multiplication { children = [e1; e2]; }) }
| e1 = expr DIV e2 = expr
    {  Ast.Operator(Ast.Division { children = [e1; e2]; }) }
| e1 = expr EQUAL e2 = expr
    { Ast.Operator(Ast.Equal {a = e1; b = e2;}) }
| e1 = expr GEQUAL e2 = expr
    { Ast.Operator(Ast.Gequal {a = e1; b = e2;}) }
| e1 = expr LEQUAL e2 = expr
    { Ast.Operator(Ast.Lequal {a = e1; b = e2;}) }
| e1 = expr GREATER e2 = expr
    { Ast.Operator(Ast.Greater {a = e1; b = e2;}) }
| e1 = expr LESS e2 = expr
    { Ast.Operator(Ast.Less {a = e1; b = e2;}) }
| e1 = expr NEQUAL e2 = expr
    { Ast.Operator(Ast.Nequal {a = e1; b = e2;}) }
| store = expr START_ACCESSOR key = expr END_ACCESSOR
    { Ast.Accessor { store; key; } }
| MINUS e = expr %prec UMINUS
    {  Ast.Operator(Ast.Negation { children = [e]; }) }
| NOT inverted = expr
    {  Ast.Operator(Ast.Not { inverted; }) }