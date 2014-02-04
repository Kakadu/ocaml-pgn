open Printf

type move_t = string
type game_tree = {
  pre_ann: string;
  post_ann: string;
  move: move_t;
  variants: game_tree list;
  aux: string list;
}

let move_of_string move = { move; pre_ann=""; post_ann=""; aux=[]; variants=[] }

let string_of_pgn_file (tags, moves, postfix) : string =
  let tags = List.map (fun (a,b) -> sprintf "%s\t%s\n" a b) tags |> String.concat "" in
  let moves_str =
    moves |> List.map (fun a -> sprintf "%s " a.move)
    |> String.concat " "
  in
  let s3 = match postfix with Some s -> s | None -> "" in
  tags ^ moves_str ^ s3

