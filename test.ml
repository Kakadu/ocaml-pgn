open Printf
open Ostap.Combinators

type options_t = { mutable verbose: bool }
let options = { verbose = false }

let () = Arg.parse 
  [("-v", Arg.Unit (fun () -> options.verbose <- true), "verbose true results")]
  (fun _ -> ()) "usage msg"


let () = Pgn.parse_file "CapablancaYaffe.pgn" 
let () = Pgn.parse_file "test1.pgn" 


let report_error ~msg line = 
  printf "Error with line `%s`: %s\n" line msg

let report_ok () = 
  if options.verbose then printf "OK!\n" else ()

let wrap_parse f tostr line expected =
  let lex = new Lexer.lexer line in
  match (f lex, expected) with
  | Parsed ((r1,_),_), Some r2 when r1=r2 -> report_ok ()
  | Parsed ((r1,_),_) , Some r2  -> 
      report_error line ~msg:(sprintf "values not equal: %s and %s" (tostr r1) (tostr r2))
  | Parsed (_,_), None     -> report_error ~msg:"Something parsed when shouldn't" line
  | (Failed _, None)  -> report_ok ()
  | (Failed r, Some _) ->
     report_error line 
       ~msg:(sprintf "Can't parse when parse result expected. %s" (Ostap.Reason.toString `All `Desc r))

let test_tagline = wrap_parse Parser.tag Parser.string_of_tagresult
let test_move    = wrap_parse Parser.move (fun x -> x)

let () = List.iter (fun (a,b) -> test_tagline a b)
  [ ("[White \"Kasparov\"]", (Some ("White","Kasparov")) )
  ; ("[Result \"1-0\"]",     (Some ("Result", "1-0")) )
  ; ("[Result \"0-1\"]",     (Some ("Result", "0-1")) )
  ; ("[Round \"5\"]",                     (Some ("Round",  "5")) )
  ; ("[Opening \"Queen's Gambit Dec.\"]", (Some ("Opening",  "Queen's Gambit Dec.")) )
  ; ("[Date \"1910.??.??\"]",             (Some ("Date",  "1910.??.??")) )
  ]

let () = List.iter (fun (a,b) -> test_move a b)
  [ ("Nbd7", Some "Nbd7")

  ]     
