open Printf

let () = Printexc.record_backtrace true

let filename = ref "test/game2.pgn"

let () =
  if Array.length Sys.argv > 1 then filename := Sys.argv.(1)

let read_lexems () =
  let ch = open_in !filename in
  let buf = Lexing.from_channel ch in
  let ans = ref [] in
  try
    while true do ans := (PgnLexer.token buf) :: !ans done;
    assert false
  with Failure "lexing: empty token" ->
    close_in ch;
    List.rev !ans

let () =
  try
    printf "Parsing `%s`\n%!" !filename;
    let lst = read_lexems () in
    let _ans = PgnParser.parse_all (Stream.of_list lst) in
    print_endline "parsed!";
    PgnParser.print_game stdout _ans
    (*assert (match Stream.peek stream with Some _ -> false | _ -> true)*)
  with exc -> print_endline "Exception";
              print_endline (Printexc.get_backtrace ());
              print_endline (Printexc.to_string exc)
