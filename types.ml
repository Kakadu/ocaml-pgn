open Printf

module Option = struct
  let get ~default = function Some x -> x | None -> default
  let iter ~f = function Some x -> f x | None -> ()
end


type move = string
type game_res = WhiteWon | BlackWon | Draw | NoResult
type tree = {
    nags: int list;
    pre_ann: string option;
    post_ann: string option;
    move: move;
    next: next_t;
    variants : tree list
}
and next_t =
  [ `Continue of tree
  | `Result   of game_res
  ]

type tag = string * string
type game = (tag list) * tree

let string_of_result = function
  | WhiteWon -> "1-0"
  | BlackWon -> "0-1"
  | Draw -> "1/2-1/2"
  | NoResult -> "*"

let result_of_string_exn = function
  | "1-0" -> WhiteWon
  | "0-1" -> BlackWon
  | "1/2-1/2" -> Draw
  | "*" -> NoResult
  | _ -> failwith "Bad argument of result_of_string_exn"

let result_of_string s =
  try Some(result_of_string_exn s)
  with Failure _ -> None

(*
type game_tree = {
  pre_ann: string;
  post_ann: string;
  move: move_t;
  variants: game_tree list;
  nags: int list;
  aux: string list;
} *)

let move_of_string move = { move; pre_ann=None; post_ann=None; variants=[]; next=`Result NoResult; nags=[] }

let move_tree_to_string root =
  let b = Buffer.create 20 in
  let add_string = Buffer.add_string b in
  let bprintf fmt = add_string (sprintf fmt) in
  let ann = function
    | None   -> ()
    | Some x -> (add_string "{ "; add_string x; add_string "}")
  in
  let rec inner {move; nags; pre_ann; post_ann; variants; next } =
    ann pre_ann;
    add_string move;
    add_string " ";
    ann post_ann;
    List.iter (fun root -> add_string "( "; inner root; add_string " )") variants;
    let () = match next with
      | `Result r ->
        Buffer.add_string b (string_of_result r);
      | `Continue tree ->
        inner tree
    in
    ()
  in
  inner root;
  Buffer.contents b

let string_of_pgn_file : game -> string = fun (tags, tree) ->
  let tags = List.map (fun (a,b) -> sprintf "%s\t%s\n" a b) tags |> String.concat "" in
  let s2 = move_tree_to_string tree in
  sprintf "%s\n%s" tags s2 (*
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
                     *)

