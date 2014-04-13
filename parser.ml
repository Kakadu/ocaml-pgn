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
  figure: "K" { King } | "Q" { Queen } | "R" {Rook} | "N" {Knight} | "B" {Bishop};
  vert: "a"{VA}|"b"{VB}|"c"{VC}|"d"{VD}|"e"{VE}|"f"{VF}|"g"{VG} | "h"{VH};
  hor:  "1"{H1}|"2"{H2}|"3"{H3}|"4"{H4}|"5"{H5}|"6"{H6}|"7"{H7}|"8"{H8};
  nag:  x:NAG => { good_nag_str (repr x) } => {
    x |> repr |> no1 |> int_of_string
  };
  move_postfix: "#" {`Checkmate} | "++" {`Checkmate} | "+" { `Check };
  pawn_promotion: -"=" figure;
  fig_hint:
      h:hor  { `Rank h }
    | v:vert { `File v }
  ;
  move_itself:
      "0-0-0" y:move_postfix? { (CastleQueenSide,y) }
    | "O-O-O" y:move_postfix? { (CastleQueenSide,y) }
    | "0-0"   y:move_postfix? { (CastleKingSide,y) }
    | "O-O"   y:move_postfix? { (CastleKingSide,y) }
    | f:figure hint:(fig_hint)? takes:("x"?) v2:vert h:hor p:move_postfix? { (* Nbxd7+ *)
        (FigureMoves(f, (v2,h), hint, Option.is_some takes),p)
    }
    | f:figure takes:("x"?) v2:vert h:hor p:move_postfix? { (* Nxe4+, Nf3# *)
        (FigureMoves(f, (v2,h), None, false), p)
    }
    | pawn:vert "x" v:vert h:hor "=" prom:figure p:move_postfix? { (* exf8=N+ *)
        (PawnTakesPromotion (pawn, (v,h), prom), p)
    }
    | pawn:vert "x" v:vert h:hor p:move_postfix? { (* cxd5+ *)
        (PawnTakes (pawn, (v,h)), p)
    } (* TODO: guards *)
    | v:vert h:hor "=" prom:figure p:move_postfix? { (* a8=Q+*)
        (PawnPromotion((v,h), prom), p)
      }
    | v:vert h:hor p:move_postfix? {
        (PawnMoves(v,h), p)
    };
  comment : "{" c:COMMENT "}" {
    let input = repr c in
    let ans = String.trim input in
    printf "`%s` ------> `%s`\n" input ans;
    ans
  };
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
  let f (x:next_t) =
    match x with
    | `Continue x -> `Continue x
    | `NullMoves x -> `NullMoves x
    | `Result _ -> failwith "Result can't be in variation without move "
  in
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
      -"(" move_tree -")"
    (* Null moves now*)
    | -"(*" xs: ((comment)? -"<>" -moveN -"." move_itself)* -")" { `NullMoves xs }
    ;

    move_tree:
      moveN   "." wmove:move wvars:(variation* )
      moveN "..." bmove:move bvars:(variation* )   next:move_tree {
        let wvars = fix_variants wvars in
        let bvars = fix_variants bvars in
        let node2 = { { bmove with variants=bvars } with next = next } in
        `Continue { { wmove with variants=wvars } with next = `Continue node2 }
      }
    | moveN "." wmove:move r:result  {
      `Continue { wmove with next = `Result r }
    }
    | moveN "..." bmove:move bvars:(variation* ) tl:move_tree {
        let f = function | `Continue x -> `Continue x
	                 | `NullMoves x -> `NullMoves x
	                 | `Result _ -> failwith "Result can't be in variation without move " in
        let bvars = List.map f bvars in
        `Continue { { bmove with next =  tl } with variants=bvars }
      }
    | moveN "." wmove:move bmove:move bvars:(variation* ) next:move_tree {
        let b = { { bmove with next = next } with variants=(fix_variants bvars) } in
        `Continue { wmove with next=`Continue b }
      }
    | moveN "." wmove:move  {
      `Continue { wmove with next = `Result NoResult }
    }
    | r:result                                       { `Result  r }
    | $ { `Result NoResult }
)

let (_: (_, string * string, _) Combinators.parse) = tag

ostap ((*
    moves: xs:(move_part+) {
	List.flatten  xs
      }; *)
    game: tags:(tag)+ initial_comment:(comment?) moves:move_tree {
      Option.iter initial_comment ~f:(printf  "Initial comment: %s\n%!");
      match moves with
      | `Continue x -> (tags, x)
      | `NullMoves _
      | `Result _   -> failwith "Something is wrong"
    }
)

ostap (
  file: game+
)


