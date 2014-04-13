open Types
open Printf

type options_t = { mutable verbose: bool }
let options = { verbose = false }

let () = Arg.parse
  [("-v", Arg.Unit (fun () -> options.verbose <- true), "verbose true results")]
  (fun _ -> ()) "usage msg"

let report_error ~msg line = printf "Error with line `%s`: %s\n" line msg

let report_ok ?msg () =
  if options.verbose then printf "OK! %s\n" (match msg with Some s -> s | None -> "") else ()

let test_string_of_move (m,s) =
  printf "%s: " s;
  let ans = string_of_move m in
  if ans = s then report_ok ()
  else report_error ~msg:(" <> " ^ ans) s
(*
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
let move_equal a b = string_equal a.Types.move b.Types.move

let test_tagline = wrap_parse Parser.tag Parser.string_of_tagresult ~iseq:tag_equal
let test_move    = wrap_parse Parser.move (fun x -> x.Types.move)   ~iseq:move_equal
let test_nag     = wrap_parse Parser.nag  (fun x -> string_of_int x) ~iseq:int_equal

 *)

let () =
  List.iter test_string_of_move
    [ (CastleKingSide,None), "O-O"
    ; (CastleQueenSide,Some `Check), "O-O-O+"
    ; (PawnMoves (VE,H4),None), "e4"
    ; (PawnPromotion ((VH,H8), Queen), Some `Checkmate), "h8=Q#"
    ; (PawnTakes (VC,(VD,H5)), None), "cxd5"
    ; (PawnTakesPromotion (VB,(VA,H1),Rook), Some `Check), "bxa1=R+"
    ; (FigureMoves (King,(VD,H2),None,false), Some `Checkmate), "Kd2#"
    ; (FigureMoves (Knight,(VE,H2),Some (`File VG),false), None), "Nge2"
    ; (FigureMoves (Rook,(VB,H6),Some (`Rank H8),true), None), "R8xb6"

    ]
