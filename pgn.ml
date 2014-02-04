open Printf
open Types
open Ostap.Combinators

let parse_file filename =
  let l = new Lexer.lexer (Ostap.Util.read filename) in
  match Parser.file l with
  | Parsed((ans,_),_) ->
      print_endline "Parsed";
      printf "Result:\n";
      List.iter (fun g -> print_endline "game"; print_endline (string_of_pgn_file g)) ans
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
  | Parsed ((m,_),_) -> printf "Move parsed: %s = %s\n" line m.move
  | Failed r -> printf "Failed! %s\n" (Ostap.Reason.toString `All `Desc r)


