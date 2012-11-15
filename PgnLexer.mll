{
(* open Genlex *)
type token = 
  | Kwd of string
  | Int of int
  | String of string
  | Ident of string
  | Comment of string
}
rule token = parse
| '\n'  { token lexbuf }
| ' '   { token lexbuf }
| '{' (['a'-'z' 'A'-'Z' '0'-'9' ' ' '.' ]+ as s) '}' { Comment s }
| '"' (['0'-'9' 'a'-'z' 'A'-'Z' '/' '-' '|' ' ' '.' ',' ]+ as str) '"'  { String str }
| ['0'-'9']+ as num { Int (int_of_string num) }
| '.'               { Kwd "." }
| '['               { Kwd "[" }
| ']'               { Kwd "]" }
| "O-O-O" as s      { Ident s }
| "O-O"   as  s     { Ident s }
| ['a'-'h' '0'-'9' 'B' 'N' 'Q' 'K' 'R' 'x' '+']+ as s  { Ident s }
| "Event"  as s { Kwd s }
| "Site"   as s { Kwd s } 
| "Date"   as s { Kwd s } 
| "Round"   as s { Kwd s } 
| "White"   as s { Kwd s } 
| "Black"   as s { Kwd s } 
| "Result"   as s { Kwd s } 
| "WhiteElo"   as s { Kwd s } 
| "BlackElo"   as s { Kwd s } 
| "ECO"         as s { Kwd s } 
| "EventDate"   as s { Kwd s }
| "1-0"         as s { Kwd s }
| "0-1"         as s { Kwd s }
| "1/2-1/2"    as  s { Kwd s }

 

