open Printf

type move_t = string
type game_tree = {
  pre_ann: string;
  post_ann: string;
  move: move_t;
  variants: game_tree list;
  nags: int list;
  aux: string list;
}

let move_of_string move = { move; pre_ann=""; post_ann=""; aux=[]; variants=[]; nags=[] }

let string_of_pgn_file (tags, moves, postfix) : string =
  let tags = List.map (fun (a,b) -> sprintf "%s\t%s\n" a b) tags |> String.concat "" in
  let moves_str =
    let string_of_move {pre_ann; post_ann; move; nags; _ } =
      let pre_str = if pre_ann="" then "" else sprintf "{ %s }" pre_ann in
      let post_str = if post_ann="" then "" else sprintf "{ %s }" post_ann in
      ([pre_str; move] @ (List.map (sprintf "$%d") nags) @ [post_str] )
		 |> String.concat " "
    in
    moves |> List.map string_of_move |> String.concat " "
  in
  let s3 = match postfix with Some s -> s | None -> "" in
  tags ^ moves_str ^ s3

