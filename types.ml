open Printf

type 'a result = [ `Ok of 'a | `Error of string ]

module Option = struct
  let get ~default = function Some x -> x | None -> default
  let iter ~f = function Some x -> f x | None -> ()
  let (>>=) x f = match x with None -> None | Some x -> f x
end

module String = struct
  include String
  let lfindi cond s =
    let len = String.length s in
    let rec loop i =
      if i>= len then None
      else if cond s.[i] then Some i
      else loop (i+1)
    in
    loop 0
  let rfindi cond s =
    let rec loop i =
      if i< 0 then None
      else if cond s.[i] then Some i
      else loop (i-1)
    in
    loop (String.length s - 1)

  let trim s =
    if String.length s <> 0 then begin
     let l = lfindi ((<>)' ') s |> Option.get ~default:0 in
     let r = rfindi ((<>)' ') s |> Option.get ~default:(String.length s) in
     String.sub s l (r-l+1)
    end else ""
end

type move = string
type game_res = WhiteWon | BlackWon | Draw | NoResult
type tree = {
    nags: int list;
    pre_ann: string option;
    post_ann: string option;
    move: move;
    next: next_t;
    variants : [ `Continue of tree | `NullMoves of (string option * move) list ] list
}
and next_t =
  [ `Continue of tree
  | `Result   of game_res
  | `NullMoves of (string option * move) list
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


let move_of_string move = { move; pre_ann=None; post_ann=None; variants=[]; next=`Result NoResult; nags=[] }

let move_tree_to_string root =
  let b = Buffer.create 20 in
  let add_string = Buffer.add_string b in
  let ann = function
    | None   -> ()
    | Some x -> (add_string "{ "; add_string x; add_string " }")
  in
  let rec inner : tree -> unit = fun {move; nags; pre_ann; post_ann; variants; next } ->
      ann pre_ann;
      add_string move;
      add_string " ";
      ann post_ann;
      List.iter (fun root -> add_string "( ";
	let () = match root with
	| `NullMoves xs ->
	  List.iter (fun (text, move) -> ann text; add_string move) xs
	| `Continue tree -> inner tree
	in
	add_string " )"
      ) variants;
      match next with
      | `Continue next -> inner next
      | `Result r -> Buffer.add_string b (string_of_result r)
      | `NullMoves _ -> add_string "{ null moves }"
  in
  inner root;
  Buffer.contents b

let string_of_pgn_file : game -> string = fun (tags, tree) ->
  let tags = List.map (fun (a,b) -> sprintf "[%s\t\"%s\"]\n" a b) tags |> String.concat "" in
  let s2 = move_tree_to_string tree in
  sprintf "%s\n%s" tags s2

module Board = struct
  type color = White | Black
  type figure = King | Queen | Bishop | Knight | Rook | Pawn
  type t = (color * figure) option array array

  let figure_to_char = function
    | King   -> 'K' | Queen  -> 'Q' | Rook -> 'R'
    | Bishop -> 'B' | Knight -> 'N' | Pawn -> 'P'

  let square_to_char = function
    | None -> ' '
    | Some (color,f) ->
      let c = figure_to_char f in
      (match color with White -> c | Black -> Char.lowercase c)

  let to_string (board: t) =
    let b = Buffer.create 100 in
    for i = 0 to 7 do
      for j=0 to 7 do
	Buffer.add_char b (square_to_char board.(j).(i))
      done
    done;
    Buffer.contents b

  let create () =
    let b = Array.init 8 (fun _ -> Array.init 8 (fun _ -> None)) in
    for j=0 to 7 do
      b.(j).(1) <- Some (White,Pawn);
      b.(j).(6) <- Some (Black,Pawn)
    done;
    b.(0).(0) <- Some (White,Rook);
    (* TODO: generate new board *)
    b

  let make_move move_str board =
    let b = Array.init 8 (fun _ -> Array.init 8 (fun _ -> None)) in
    for i=0 to 7 do
      for j=0 to 7 do
	b.(i).(j) <- board.(i).(j);
      done
    done;
    printf "Making move `%s` without changing board\n" move_str;
    Some b
end

let validate_game root =

  let (>>=) = Option.(>>=) in
  let rec helper (board: Board.t option) root =
    let init: Board.t option  = board >>= Board.make_move root.move in
    let f : Board.t option -> _ -> Board.t option =
      fun acc -> function
      | `Continue x -> (helper acc x)   (* TODO *)
      | `NullMoves _ -> acc
    in
    List.fold_left f init root.variants
  in
  helper (Some (Board.create ())) root

