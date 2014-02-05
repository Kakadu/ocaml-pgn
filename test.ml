open Printf
open Ostap.Combinators

type options_t = { mutable verbose: bool }
let options = { verbose = false }

let () = Arg.parse
  [("-v", Arg.Unit (fun () -> options.verbose <- true), "verbose true results")]
  (fun _ -> ()) "usage msg"



let report_error ~msg line =
  printf "Error with line `%s`: %s\n" line msg

let report_ok () =
  if options.verbose then printf "OK!\n" else ()

let wrap_parse: ( (_, 'a, _) parse) -> iseq:('a -> 'a -> bool) -> ('a -> string) -> (string * 'a option) -> unit
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
(*
let (_:int) = wrap_parse
*)
let string_equal (s:string) (s2:string) = (s=s2)
let int_equal (x:int) y = (x=y)
let tag_equal (a,b) (c,d) = (string_equal a c) && (string_equal b d)
let move_equal a b = string_equal a.Types.move b.Types.move

let test_tagline = wrap_parse Parser.tag Parser.string_of_tagresult ~iseq:tag_equal
let test_move    = wrap_parse Parser.move (fun x -> x.Types.move)   ~iseq:move_equal
let test_nag     = wrap_parse Parser.nag  (fun x -> string_of_int x) ~iseq:int_equal

let () = List.iter (test_tagline)
  [ ("[White \"Kasparov\"]", (Some ("White","Kasparov")) )
  ; ("[Result \"1-0\"]",     (Some ("Result", "1-0")) )
  ; ("[Result \"0-1\"]",     (Some ("Result", "0-1")) )
  ; ("[Round \"5\"]",                     (Some ("Round",  "5")) )
  ; ("[Opening \"Queen's Gambit Dec.\"]", (Some ("Opening",  "Queen's Gambit Dec.")) )
  ; ("[Date \"1910.??.??\"]",             (Some ("Date",  "1910.??.??")) )
  ]

let () = List.iter test_move
  [ ("Nbd7",  Some (Types.move_of_string "Nbd7"))
  ; ("e4",    Some (Types.move_of_string "e4"))
  ; ("Rhxd8", Some (Types.move_of_string "Rhxd8"))
  ]

let () = List.iter test_nag [ ("$111", Some 111) ]

(*
let () = Pgn.parse_file "CapablancaYaffe.pgn"

let () = Pgn.parse_file "test1.pgn"

let () = Pgn.parse_file "game.pgn"

let () = Pgn.parse_file "game2.pgn"
*)
let () = Pgn.parse_file "ChigorinSteinitz.pgn"

