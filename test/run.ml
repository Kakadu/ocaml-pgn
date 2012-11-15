let () = Printexc.record_backtrace true

let filename = "test/game2.pgn"
let read_lexems () =
  let ch = open_in filename in
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
    let lst = read_lexems () in
    print_endline "aa";
    let _ans = PgnParser.parse_all (Stream.of_list lst) in
    print_endline "parsed!";
    PgnParser.print_game stdout _ans
    (*assert (match Stream.peek stream with Some _ -> false | _ -> true)*)
  with exc -> print_endline "Exception";
              print_endline (Printexc.get_backtrace ());
              print_endline (Printexc.to_string exc)
        

