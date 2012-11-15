open PgnLexer
open Printf

let parse_info_item = parser
  | [< 'Kwd "["; 'Kwd a; 'String s; 'Kwd "]" >] -> (a,s)

type move = string
type comment = string
type game_move = 
  { n:int; 
    white : move*(comment option); 
    black : move*(comment option)
  }

type t = 
  | Item of (string*string)
  | Game of game_move list

let print_game ch game =
  List.iter (function
    | Item (s,s2) -> Printf.fprintf ch "%s: %s\n" s s2
    | Game xs -> 
        List.iter (fun {n; white=(w,c1); black=(b,c2) } -> 
          Printf.fprintf ch "%d. %s %s %s %s\n" 
            n w (match c1 with None -> "" | Some s -> sprintf "{%s}" s)
              b (match c2 with None -> "" | Some s -> sprintf "{%s}" s) 
        ) xs
  ) game;
  flush ch

let rec not_bra = parser 
  | [< 'Kwd "}" >] -> []
  | [< 'Ident s;  x=not_bra >] -> s::x
  | [< 'String s; x=not_bra >] -> s::x
  | [< 'Kwd s   ; x=not_bra >] -> s::x

let parse_result = parser
  | [< 'Kwd "1-0" >] -> []
  | [< 'Kwd "0-1" >] -> []
  | [< 'Kwd "1/2-1/2" >] -> []

let rec parse_game = parser
  | [< e=parse_result >] -> []
  | [< 'Int n; 'Kwd "."; 'Ident w; stream >] -> 
      (parser
        | [< 'Ident b; tail=parse_game >] -> 
            let white=(w,None) and black=(b,None) in
            {n; white; black} :: tail
        | [< 'Comment s; 'Int nn when nn=n; 
             'Kwd "."; 'Kwd "."; 'Kwd "."; 'Ident b; stream >] ->
             (parser
               | [< 'Comment s2; tail=parse_game >] -> 
                   let white = (w,Some s) and black = (b,Some s2) in
                   {n; white; black}::tail
               | [< tail=parse_game >] -> 
                   {n; white=(w,Some s); black=(b,None) } :: tail
             ) stream
        | [< >] -> [ {n;white=(w,None); black=("",None)} ]
      ) stream

let rec parse_all = parser
  | [< game=parse_game>] -> [Game game]
  | [< item=parse_info_item; tail=parse_all >] -> (Item item)::tail

