{
  open Parser
  open Core.Std

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| ';'
    { SEMICOLON }
| ['A'-'Z''a'-'z']+ as id
    { ID (id) }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
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
| "🤡"
    { VAR_DEC }
| "🍬"
    { RETURN }
| "😱"
    { ASSIGN }
| "🕸"
    { COMMA }
| "💀"
    { LBRACE }
| "☠️"
    { LBRACE }
| "😈"
    { LPAREN }
| "👿"
    { RPAREN }
| "👻 "
    { FUNC }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }