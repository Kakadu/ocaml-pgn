open Printf
open Ostap
open Types


let repr = Matcher.Token.repr
let make_reason msg l = new Reason.t (Msg.make msg [||] (Matcher.Token.loc l))

let is_digit = function '0'..'9' -> true  | _ -> false
let good_nag_str s =
  let len = String.length s in
  (len >=2) && (len <=4) && (
    let ans = ref true in
    for i=2 to len-1 do ans := !ans && is_digit s.[i] done;
    !ans
   )

(* convert result of parsing to string *)
let string_of_tagresult (a,b) = sprintf "(%s, %s)" a b

let remove_quotes s  =
  let len = String.length s in
  if len < 2 then failwith "Wrong argument of remove_quotes"
  else String.sub s 1 (len-2)

let no1 s = String.sub s 1 (String.length s - 1)

let () = ()

ostap (
  figure: a:("K" | "Q" | "R" | "N" | "B") { repr a };
  vert: x:("a"|"b"|"c"|"d"|"e"|"f"|"g"|"h") { repr x };
  nag:  x:NAG => { good_nag_str (repr x) } => {
    x |> repr |> no1 |> int_of_string
  };
  hor:  x:("1"|"2"|"3"|"4"|"5"|"6"|"7"|"8")  { repr x };
  move_postfix: "#" { () } | "+" {()} | -"=" figure {()} | "++" { () };
  move_itself:
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
    };
  comment : "{" c:COMMENT "}" { repr c };
  move:
    pre_ann:(comment?)  move:move_itself nags:( nag* ) post_ann:(comment?)  {
      (*printf "move %s is parsed. nags length = %d\n%!" m (List.length all_nags); *)
      { move = move
      ; nags = nags
      ; pre_ann = pre_ann
      ; post_ann = post_ann
      ; variants = []
      ; next = Obj.magic 1
      }
    }
)

ostap (
  result:
    "1-0" { WhiteWon }
  | "0-1" { BlackWon }
  | "1/2-1/2" { Draw }
  | "*"   { NoResult }
)

(*
let (_: (_, Types.tree, _) Ostap.Combinators.parse) = move_tree
*)

let fix_variants =
  let f = function `Continue x -> x | `Result _ -> failwith "Result can't be in variation without move " in
  List.map f

ostap (
    result_in_quotes: x:("\"1-0\"" | "\"0-1\"" | "\"1/2-1/2\"" | "\"S-S\"") {
      x |> repr |> remove_quotes
    };

    moveN: x:LITERAL { x |> repr |> int_of_string };

    tag: "[" "Result" r:result_in_quotes "]"           { ("Result", r) }
       | "[" x:TAGNAME y:STRINGINQUOTES "]"  { (repr x, y |> repr |> remove_quotes) };
    game_postfix:
      r:result { (*print_endline "Game_postfix 1 parsed"; *)Some r }
    | "*"      { None }
    | $        { (*print_endline "Game_postfix 3 parsed"; *)None } ;

    variation:
      -"(" move_tree -")" ;

    move_tree:
      moveN   "." wmove:move wvars:(variation* )
      moveN "..." bmove:move bvars:(variation* )   next:move_tree {
        let f = function `Continue x -> x | `Result _ -> failwith "Result can't be in variation without move " in
        let wvars = List.map f wvars in
        let bvars = List.map f bvars in
        let node2 = { { bmove with variants=bvars } with next = next } in
        `Continue { { wmove with variants=wvars } with next = `Continue node2 }
      }
    | moveN "." wmove:move r:result  {
      `Continue { wmove with next = `Result r }
    }
    | moveN "..." bmove:move bvars:(variation* ) tl:move_tree {
        let f = function `Continue x -> x | `Result _ -> failwith "Result can't be in variation without move " in
        let bvars = List.map f bvars in
        `Continue { { bmove with next =  tl } with variants=bvars }
      }
    | moveN "." wmove:move bmove:move bvars:(variation* ) next:move_tree {
        let b = { { bmove with next = next } with variants=(fix_variants bvars) } in
        `Continue { wmove with next=`Continue b }
      }
    | r:result                                       { `Result  r }
    | $ { `Result NoResult }
)

let (_: (_, string * string, _) Combinators.parse) = tag

ostap ((*
    moves: xs:(move_part+) {
	List.flatten  xs
      }; *)
    game: tags:(tag)+ initial_comment:comment? moves:move_tree {
      Option.iter initial_comment ~f:(printf  "Initial comment: %s\n%!");
      match moves with
      | `Continue x -> (tags, x)
      | `Result _   -> failwith "Something is wrong"
    }
)

ostap (
  file: game+
)


