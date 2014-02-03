open Printf 

let string_of_pgn_file (tags, moves, postfix) : string =
  let tags = List.map (fun (a,b) -> sprintf "%s\t%s\n" a b) tags |> String.concat "" in
  let moves_str = 
    moves |> List.map (fun (a) -> sprintf "%s " a) 
    |> String.concat " " 
  in
  let s3 = match postfix with Some s -> s | None -> "" in
  tags ^ moves_str ^ s3

