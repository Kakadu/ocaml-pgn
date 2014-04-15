open Printf
open Ostap.Combinators
open Types

type options_t = { mutable verbose: bool }
let options = { verbose = false }

let () = Arg.parse
  [("-v", Arg.Unit (fun () -> options.verbose <- true), "verbose true results")]
  (fun _ -> ()) "usage msg"



let report_error ~msg line =
  printf "Error with line `%s`: %s\n" line msg

let report_ok () =
  if options.verbose then printf "OK!\n" else ()

let wrap_parse
: ( (_, 'a, _) parse) -> iseq:('a -> 'a -> bool) -> ('a -> string) -> (string * 'a option) -> unit
= fun parser_f ~iseq tostr (line, expected) ->
  let lex = new Lexer.lexer line in
  match (parser_f lex, expected) with
  | ((Parsed ((r1,_),_)), Some r2) when iseq r1 r2 -> report_ok ()
  | ((Parsed ((r1,_),_)), Some r2)  ->
      report_error line ~msg:(sprintf "values not equal: `%s` and `%s`" (tostr r1) (tostr r2))
  | (Parsed (_,_)), None     -> report_error ~msg:"Something parsed when shouldn't" line
  | ((Failed _), None)  -> report_ok ()
  | ((Failed r), Some _) ->
     report_error line
       ~msg:(sprintf "Can't parse when parse result expected. %s" (Ostap.Reason.toString `All `Desc r))

let string_equal (s:string) (s2:string) = (s=s2)
let int_equal (x:int) y = (x=y)
let tag_equal (a,b) (c,d) = (string_equal a c) && (string_equal b d)

let test_tagline = wrap_parse Parser.tag Parser.string_of_tagresult ~iseq:tag_equal
let test_move    = wrap_parse Parser.move_itself string_of_move   ~iseq:move_equal
let test_nag     = wrap_parse Parser.nag  string_of_int    ~iseq:int_equal

let () = List.iter (test_tagline)
  [ ("[White \"Kasparov\"]", (Some ("White","Kasparov")) )
  ; ("[Result \"1-0\"]",     (Some ("Result", "1-0")) )
  ; ("[Result \"0-1\"]",     (Some ("Result", "0-1")) )
  ; ("[Round \"5\"]",                     (Some ("Round",  "5")) )
  ; ("[Opening \"Queen's Gambit Dec.\"]", (Some ("Opening",  "Queen's Gambit Dec.")) )
  ; ("[Date \"1910.??.??\"]",             (Some ("Date",  "1910.??.??")) )
  ]

let () =
  List.iter test_move
  [ ("Nbd7", Some (FigureMoves (Knight, (VD,H7), Some(`File VB), false), None ) )
  ; "e4", Some (PawnMoves (VE,H4),None)
  ; "Nf3", Some (FigureMoves (Knight, (VF,H3), None, false), None)
  ; "Rhxd8", Some (FigureMoves (Rook, (VD,H8), Some (`File VH), true), None)
  ]

let () = List.iter test_nag [ ("$111", Some 111) ]

(* testing my StdLib *)
let () =
  let error a b msg = printf "Trim failed: \"%s\", \"%s\": %s\n%!" a b msg in
  let trim_test (a,b) =
    let p  = String.trim a in
    if p <> b then error a b (sprintf  "results not equal (got `%s`)" p)
    else report_ok ()
  in
  List.iter trim_test
    [ "abcd", "abcd"
    ; " qwe ", "qwe"
    ; "Myers", "Myers"
    ; " x y z ", "x y z"
    ]
(*
let () = Pgn.parse_file "game.pgn"
 *)
(*
let () = Pgn.parse_file "CapablancaYaffe.pgn"

let _ = exit 0

 *)
(*
let () = Pgn.parse_file "game2.pgn" *)

let () = Pgn.parse_file "musketeer.pgn"

(*
let () = Pgn.parse_file "ChigorinSteinitz.pgn"
*)
