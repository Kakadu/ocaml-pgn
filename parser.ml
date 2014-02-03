open Printf
open Ostap

module Option = struct
  let get ~default = function Some x -> x | None -> default
end

let repr = Matcher.Token.repr
let make_reason msg l = new Reason.t (Msg.make msg [||] (Matcher.Token.loc l))

type move_t = string
type game_tree = {
  pre_ann: string;
  post_ann: string;
  move: move_t;
  variants: game_tree list;
}
(* convert result of parsing to string *)
let string_of_tagresult (a,b) = sprintf "(%s, %s)" a b
let remove_quotes s = 
  let len = String.length s in
  if len < 2 then failwith "Wrong argument of remove_quotes"
  else String.sub s 1 (len-2)

ostap (
  figure: a:("K" | "Q" | "R" | "N" | "B") { repr a };
  vert: x:("a"|"b"|"c"|"d"|"e"|"f"|"g"|"h") { repr x };
  (*hor:  x:HORIZ { repr x };*)
  hor:  x:("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8")  { repr x };
  move_postfix: "#" { () } | "+" {()} | -"=" figure {()} | "++" { () };
  move: 
      x:"0-0" move_postfix? { repr x }
    | x:"O-O" move_postfix? { repr x }
    | x:"0-0-0" move_postfix? { repr x }
    | x:"O-O-O" move_postfix? { repr x }
    | pawn:vert "x" v:vert h:hor p:move_postfix? { (* pawn takes *)
        sprintf "%sx%s%s" pawn v h 
      }
    | f:figure v1:vert v2:vert h:hor p:move_postfix? { (* Nbd7+ *)
        sprintf "%s%s%s%s" f v1 v2 h
    }
    | f:(figure?) takes:("x"?) v:vert h:hor p:move_postfix? { 
        sprintf "%s%s%s%s" (Option.get ~default:"" f) 
          (match takes with Some x -> repr x | None -> "") v h 
    }
 
)

let (_: (_, string,_) Ostap.Combinators.parse) = move

ostap (
    result_in_quotes: x:("\"1-0\"" | "\"0-1\"" | "\"1/2-1/2\"" | "\"S-S\"") { 
      x |> repr |> remove_quotes 
    };
    result: x:("1-0" | "0-1" | "S-S") { x |> repr };
    
    tag: "[" "Result" r:result_in_quotes "]"           { ("Result", r) }
       | "[" x:TAGNAME y:STRINGINQUOTES "]"  { (repr x, y |> repr |> remove_quotes) };
    game_postfix:
      r:result { print_endline "Game_postfix 1 parsed"; Some r }
    | "*"      { None }
    | $        { print_endline "Game_postfix 3 parsed"; None } ; 
    move_part:
      LITERAL "." l:move r:move ("#"?)  { [l;r] }
    | LITERAL "." l:move ("#"?)         { [l]   }   
)

let (_: (_,string list, _) Combinators.parse) = move_part 

ostap (
    moves: xs:(move_part+) { 
	let (_: string list list) = xs in
	List.flatten  xs 
    };
    game: tags:(tag)+ moves:moves xx:game_postfix {
      let (_:string option) = xx in 
      let (_:string list) = moves in
      (tags, moves, xx) 
    }
) 

let file =
  let ostap (
    file: game+
  ) in
  file

