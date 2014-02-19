open Sexplib.Conv
open Printf

type 'a result = [ `Ok of 'a | `Error of string ]

let string_of_char c = let ans = " " in ans.[0] <- c; ans

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
  let index_exn = index
  let index s c = try Some(index_exn s c) with Not_found -> None
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
  type color = White | Black with sexp
  type figure = King | Queen | Bishop | Knight | Rook | Pawn with sexp
  type vert = VA|VB|VC|VD|VE|VF|VG|VH with sexp
  type horizontal = H1|H2| H3|H4|H5|H6|H7|H8 with sexp
  type board_cell_content = (color*figure) option with sexp
  type t = board_cell_content array array with sexp

  let figure_to_char = function
    | King   -> 'K' | Queen  -> 'Q' | Rook -> 'R'
    | Bishop -> 'B' | Knight -> 'N' | Pawn -> 'P'

  let square_to_char sq =
    match sq with
    | None -> '.'
    | Some (color,f) ->
      let c = figure_to_char f in
      (match color with White -> c | Black -> Char.lowercase c)

  let copy board =
    let b = Array.init 8 (fun _ -> Array.init 8 (fun _ -> None)) in
    for i=0 to 7 do
      for j=0 to 7 do
	b.(i).(j) <- board.(i).(j);
      done
    done;
    b

  let to_string (board: t) =
    let b = Buffer.create 100 in
    for i = 7 downto 0 do
      for j=0 to 7 do
	Buffer.add_char b (square_to_char board.(j).(i))
      done;
      Buffer.add_char b '\n';
    done;
    Buffer.contents b

  let int_of_horizontal h =
    (match h with
    | H1 -> 1 | H2 -> 2 | H3 -> 3 | H4 -> 4 | H5 -> 5 | H6 -> 6 | H7 -> 7 | H8 -> 8) - 1
  let int_of_vertical v =
    (match v with
    | VA -> 1 | VB -> 2 | VC -> 3 | VD -> 4 | VE -> 5 | VF -> 6 | VG -> 7 | VH -> 8) - 1

  let set b (v,h) content =
    b.(int_of_vertical v).(int_of_horizontal h) <- content

  (* Place [v] to square [cell] on board [b]. [cell] is a string of length 2 *)
  let set_unsafe b cell v =
    if String.length cell <> 2 then failwith "bad arguments of set_unsafe";
    let code0 = Char.code cell.[0] in
    if Char.(code0 < code 'a' || code0 > code 'h')
    then failwith "bad argument (vertical) of set_unsafe";
    let code1 = Char.code cell.[1] in
    if Char.(code1 < code '1' || code1 > code '8')
    then failwith "bad argument (horizontal) of set_unsafe";
    let ans = copy v in
    ans.(Char.(code0 - code 'a')).(Char.(code1 - code '1')) <- Some v;
    ans

  let get_cell_value b (v,h) = b.(int_of_vertical v).(int_of_horizontal h)
  let empty_cell b (v,h) =
    match get_cell_value b (v,h) with Some _ -> false | None -> true

  let set_cell_value b (v,h) data = b.(int_of_vertical v).(int_of_horizontal h) <- data

  let empty_cell_unsafe b cell =
    match b.(Char.code cell.[0] - Char.code 'a').(Char.code cell.[1] - Char.code '1') with
    | Some x -> false
    | None -> true

  let create () =
    let b = Array.init 8 (fun _ -> Array.init 8 (fun _ -> None)) in
    for j=0 to 7 do
      b.(j).(1) <- Some (White,Pawn);
      b.(j).(6) <- Some (Black,Pawn)
    done;
    b.(0).(0) <- Some (White,Rook);
    (* TODO: generate new board *)
    b

  let is_vert c = Char.(code c >= code 'a' && code c <= code 'h')
  let is_horiz c = Char.(code c >= code '1' && code c <= code '8')
  let inverse_color = function White -> Black | Black -> White

  let cell_of_string s =
    let v = match s.[0] with
      | 'a' -> VA | 'b' -> VB | 'c' -> VC | 'd' -> VD | 'e' -> VE | 'f' -> VF | 'g' -> VG | 'h' -> VH
      | c  -> failwith (sprintf "Cant parse vertical of char '%c'" c)
    in
    let h = match int_of_string @@ string_of_char s.[1] with
      | 1 -> H1 | 2 -> H2 | 3 -> H3 | 4 -> H4 | 5 -> H5 | 6 -> H6 | 7 -> H7 | 8 -> H8
      | c  -> failwith (sprintf "Cant parse horizontal of int '%d'" c)
    in
    (v,h)

  let make_cell c1 c2 = let ans = "11" in ans.[0]<-c1; ans.[1]<-c2; ans
  let get_content b cell = b.(Char.code cell.[0] - Char.code 'a').(Char.code cell.[1] - Char.code '1')

  (* [board] is mutable copy of board which will be returned as answer
   * [color] White or Black
   * [figure]  is which piece moves. (pawns not included)
   * [takes] is boolean flags which indicates that piece is taking another peice
   * [helper] additional info about piece, for example 'a' in Rab1
   * [dest] is destination cell on the board.
   **)
  let move_figure board color figure takes ?helper dest =
    None

  let make_move: string -> (color * t) -> (color * t) option = fun move_str (side_color, board) ->
    let b = copy board in
    printf "make_move '%s': board is:\n%s\n" move_str (to_string b);
    printf "sexp board %s\n%!" (Sexplib.Sexp.to_string_hum @@ sexp_of_t b);
    let is_pawn_move = (String.length move_str=2) && is_vert move_str.[0] && is_horiz move_str.[1] in
    let is_figure_move () =
      let takes = String.index move_str 'x' <> None in
      let figure =
        match move_str.[0] with
        | 'B' -> Bishop | 'N' -> Knight | 'R' -> Rook | 'Q' -> Queen | 'K' -> King | _ -> failwith "impossible"
      in
      let dest = (VA,H1) in
      (figure,takes,dest)
    in

    if is_pawn_move && side_color=White then begin
      let (v,h) = cell_of_string move_str in
      if h = H4 then begin
        let e2 = (v,H2) in
        let e3 = (v,H3) in
        let e4 = (v,H4) in
        if empty_cell b e3 && (get_cell_value b e2 = Some (White,Pawn)) then begin
          (* pawn move from initial position *)
          set_cell_value b e2 None;
          set_cell_value b e4 (Some(White,Pawn));
          Some (inverse_color side_color, b)
        end else if empty_cell b e2 && (get_cell_value b e3 = Some (White,Pawn)) then begin
          set_cell_value b e3 None;
          set_cell_value b e4 (Some(White,Pawn));
          Some (inverse_color side_color, b)
        end else failwith (sprintf "Can't make pawn move '%s'" move_str)
      end else begin
        failwith "TODO: implement white moves on black's board"
      end
    end else if is_pawn_move && side_color=Black then begin
      let (v,h) = cell_of_string move_str in
      match h with
      | H5 ->
        let e7 = (v,H7) in
        let e6 = (v,H6) in
        let e5 = (v,H5) in
        if empty_cell b e6 && (get_cell_value b e7 = Some (Black,Pawn)) then begin
          (* pawn move from initial position *)
          set_cell_value b e7 None;
          set_cell_value b e5 (Some(White,Pawn));
          Some (inverse_color side_color, b)
        end else if empty_cell b e7 && (get_cell_value b e6 = Some (White,Pawn)) then begin
          set_cell_value b e6 None;
          set_cell_value b e5 (Some(White,Pawn));
          Some (inverse_color side_color, b)
        end else failwith (sprintf "Can't make pawn move '%s'" move_str)
      | H6|H4|H3|H2 ->
        let prev_hor = match h with  H6->H7 | H4->H5 | H3 -> H4 | H2->H3 | _ -> assert false in
        if get_cell_value b (v,prev_hor) = Some (Black,Pawn) && empty_cell b (v,h) then begin
          set_cell_value b (v,prev_hor) None;
          set_cell_value b (v,h)        (Some (Black,Pawn));
          Some (inverse_color side_color,b)
        end else begin
          failwith "TODO: implement white moves on black's board"
        end
      | _ -> failwith "impossible"
    end else begin
      let (figure,takes,dest) = is_figure_move () in
      move_figure b side_color figure takes dest
    end
end

let validate_game root =
  let (>>=) = Option.(>>=) in
  let rec helper (board: (Board.color * Board.t) option) root =
    let init: (Board.color * Board.t) option  = board >>= Board.make_move root.move in
    let f : (Board.color * Board.t) option -> _ -> (Board.color * Board.t) option =
      fun acc -> function
      | `Continue x -> (helper acc x)   (* TODO *)
      | `NullMoves _ -> acc
    in
    (match root.next with
    | `NullMoves _ -> init
    | `Result _ -> init
    | `Continue root2 -> helper init root2) >>= fun _ -> List.fold_left f init root.variants
  in
  helper (Some (Board.White, Board.create ())) root

