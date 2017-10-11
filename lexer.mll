{
  open Parser
  open Core

  exception Error of string

  let position lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
        Printf.sprintf "%d:%d" 
        p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

  let error lexbuf fmt = 
      Printf.ksprintf (fun msg -> 
          raise (Error ((position lexbuf)^" "^msg))) fmt
}

let ws    = [' ' '\t']
let nl    = '\n'
let digit = ['0'-'9']
let digits = digit+



rule token = parse
| ws+
    { token lexbuf }
| nl
    { Lexing.new_line lexbuf; token lexbuf }
| '"'
    { STR (string (Buffer.create 100) lexbuf) }
| "🌚"
    { VOID }
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
    { error lexbuf "Eek! This text is too scary to lex: %s." @@ Lexing.lexeme lexbuf }

and string buf = parse
| [^'"' '\n' '\\']+  
  { Buffer.add_string buf @@ Lexing.lexeme lexbuf;
    string buf lexbuf 
  }
| '\n'
  { Buffer.add_string buf @@ Lexing.lexeme lexbuf;
    Lexing.new_line lexbuf;
    string buf lexbuf
  }
| '\\' '"'
  { Buffer.add_char buf '"';
    string buf lexbuf
  }
| '\\'
  { Buffer.add_char buf '\\';
    string buf lexbuf
  }
| '"'
  { Buffer.contents buf }
| eof
  { error lexbuf "AHHHH! You forgot to close a string! We're so scared that we crashed!" }
| _
  { error lexbuf "I'm gonna pass out! We found some crazy ass character inside of a string! It looks like: %s! Kill it!" @@ Lexing.lexeme lexbuf }