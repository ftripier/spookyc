{
  open Parser
  open Core

  exception Error of string
}

let ws    = [' ' '\t']
let nl    = ['\n']
let digit = ['0'-'9']
let digits = digit+

rule token = parse
| ws+
    { token lexbuf }
| nl
    { Lexing.new_line lexbuf; token lexbuf }
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
| (digits)'.'?(digits)* as i
    { NUMBER (float_of_string i) }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }