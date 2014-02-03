open PgnLexer
open Printf

let parse_info_item = parser
  | [< 'Kwd "["; 'Kwd a; 'String s; 'Kwd "]" >] -> (a,s)

type move = string
type comment = string
type color = [ `White | `Black ]
type game_move =
  { n:int;
    white : move*(comment option);
    black : move*(comment option)
  }

type game_result = WhiteWon | BlackWon | Draw
let string_of_result = function
  | WhiteWon -> "1-0"
  | BlackWon -> "0-1"
  | Draw     -> "1/2-1/2"

type parse_result =
  | Item of (string * string)
  | Game of game_move list * game_result option

let print_game ch game =
  List.iter (function
    | Item (s,s2) -> Printf.fprintf ch "%s: %s\n" s s2
    | Game (xs,r) ->
        List.iter (fun {n; white=(w,c1); black=(b,c2) } ->
          Printf.fprintf ch "%d. %s %s %s %s\n"
            n w (match c1 with None -> "" | Some s -> sprintf "{%s}" s)
              b (match c2 with None -> "" | Some s -> sprintf "{%s}" s)
        ) xs;
        print_endline (match r with
        | Some r -> string_of_result r
        | None -> "*")
  ) game;
  flush ch

let rec not_bra = parser
  | [< 'Kwd "}" >] -> []
  | [< 'Ident s;  x=not_bra >] -> s::x
  | [< 'String s; x=not_bra >] -> s::x
  | [< 'Kwd s   ; x=not_bra >] -> s::x

let parse_result = parser
  | [< 'Kwd "1-0" >] -> Some WhiteWon
  | [< 'Kwd "0-1" >] -> Some BlackWon
  | [< 'Kwd "1/2-1/2" >] -> Some Draw
  | [< 'String "*" >]    -> None
  | [< >] -> None

let rec parse_game = parser
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
  | [< game=parse_game; result=parse_result >] -> [Game (game, result)]
  | [< game=parse_game   >]                    -> [Game (game, None)]
  | [< item=parse_info_item; tail=parse_all >] -> (Item item)::tail

let parse_string s =
  let buf = Lexing.from_string s in
  let ans = ref [] in
  try
    while true do ans := (PgnLexer.token buf) :: !ans done;
    assert false
  with Failure "lexing: empty token" ->
         parse_all (Stream.of_list !ans)

let read_lexems filename =
  let ch = open_in filename in
  let buf = Lexing.from_channel ch in
  let ans = ref [] in
  try
     while true do ans := (PgnLexer.token buf) :: !ans done;
     assert false
  with Failure "lexing: empty token" ->
       close_in ch;
       List.rev !ans

type game = 
  { info:   (string*string) list
  ; moves:  game_move list
  ; result: game_result option
  }

let games_of_parse_result r = 
  let rec helper info_acc res_acc xs = 
    if xs = [] then ( (*
      assert (List.length moves_acc > 0);*)
      res_acc
    ) else (
      match xs with
      | (Item x)::tl -> helper (x::info_acc) res_acc tl
      | (Game (moves, result))::tl -> helper [] ({info=info_acc;moves;result}::res_acc) tl
      | [] -> res_acc
    )
  in
  helper [] [] r

let parse_file name =
  let lst = read_lexems name in
  games_of_parse_result (parse_all (Stream.of_list lst))


