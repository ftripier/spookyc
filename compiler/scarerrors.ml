open Core

exception Error of string

let position lexbuf =
  let p = lexbuf.Lexing.lex_curr_p in
      Printf.sprintf "line %d: position %d -" 
      p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let error lexbuf fmt = 
    Printf.ksprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt