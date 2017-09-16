{
  open Parser
  open Core.Std

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| "👻"
    { FUNC }
| "🤡"
    { VAR_DEC }
| "🍬"
    { RETURN }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| "🎃"
    { SEMICOLON }
| "😱"
    { ASSIGN }
| "🕸️"
    { COMMA }
| "👿"
    { LPAREN }
| "😈"
    { RPAREN }
| "💀"
    { LBRACE }
| "☠️"
    { RBRACE }
| ['A'-'Z''a'-'z''_''!']['A'-'Z''a'-'z''_''0'-'9''!']*  as id
    { ID (id) }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }