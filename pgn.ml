open Printf
open Types
open Ostap.Combinators

let parse_file filename =
  let l = new Lexer.lexer (Ostap.Util.read filename) in
  match Parser.file l with
  | Parsed((ans,_),msg) ->
      print_endline "Parsed";
      printf "Result:\n";
      List.iter (fun g ->
	print_endline "Validating game...";
	let () = match Types.validate_game (snd g) with
        | Some b -> printf "validated!\n%!"
        | None   -> printf "validation error!\n%!"
        in
	print_endline (string_of_pgn_file g)
      ) ans;
      printf "message: %s" (Ostap.Reason.toString `All `Desc msg)
  | Failed r  ->
      printf "File `%s` failed" filename;
      print_endline (Ostap.Reason.toString `All `Desc r)

let parse_tag_line line =
  let l = new Lexer.lexer line in
  match Parser.tag l with
  | Parsed (((key,v),_),_) ->
      printf "parsed: (%s,%s)\n" key v;
  | Failed r ->
      printf "Failed! %s\n" (Ostap.Reason.toString `All `Desc r)

let parse_move line =
  let l = new Lexer.lexer line in
  match Parser.move l with
  | Parsed ((m,_),_) -> printf "Move parsed: %s = %s\n" line (string_of_move m.move)
  | Failed r -> printf "Failed! %s\n" (Ostap.Reason.toString `All `Desc r)


