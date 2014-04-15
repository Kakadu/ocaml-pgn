open Sexplib.Conv
open Printf
open Sexplib.Conv
open Printf

type 'a result = [ `Ok of 'a | `Error of string ]

let string_of_char c = let ans = " " in ans.[0] <- c; ans

let with_buffer ?size f =
  let b = Buffer.create 10 in
  f b;
  Buffer.contents b

module Option = struct
  let get ~default = function Some x -> x | None -> default
  let iter ~f = function Some x -> f x | None -> ()
  let (>>=) x f = match x with None -> None | Some x -> f x
  let (>|=) x f = match x with None -> None | Some x -> Some (f x)
  let is_some = function Some _ -> true | None  -> false
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

type color = White | Black with sexp
type figure = King | Queen | Bishop | Knight | Rook | Pawn with sexp
(* It will be great to have something like enumerable there *)
type file = VA|VB|VC|VD|VE|VF|VG|VH with sexp (* вертикаль *)
type rank = H1|H2|H3|H4|H5|H6|H7|H8 with sexp (* горизонталь *)
type cell = file*rank

let string_of_color = function White -> "White" | Black -> "Black"
let figure_to_char = function
  | King   -> 'K' | Queen  -> 'Q' | Rook -> 'R'
  | Bishop -> 'B' | Knight -> 'N' | Pawn -> 'P'
let char_of_figure = figure_to_char

let int_of_file = function
  | VA -> 0 | VB -> 1 | VC -> 2 | VD->3 | VE-> 4 | VF->5 | VG->6 | VH->7
let int_of_rank = function
  | H1 -> 0 | H2 -> 1 | H3 -> 2 | H4->3 | H5-> 4 | H6->5 | H7->6 | H8->7

let near_files f1 f2 = abs (int_of_file f1 - (int_of_file f2)) = 1

let char_of_file v = Char.chr (Char.code 'a' + (int_of_file v))
let char_of_rank h = Char.chr (Char.code '1' + (int_of_rank h))
let string_of_cell (v,h) = sprintf "%c%c" (char_of_file v) (char_of_rank h)

type mm_postfix = [`Check | `Checkmate ] option
type mm_takes = bool
type mm_fig_hint = [ `File of file | `Rank of rank ] option
type move_kind =
  | CastleKingSide
  | CastleQueenSide
  | FigureMoves of figure * cell * mm_fig_hint * mm_takes
  | PawnTakes of file*cell (* vert*cell *)
  | PawnTakesPromotion of file*cell* figure
  (* TODO: Pawn can't promote to King. Use PolyVariants *)
  | PawnMoves of cell (* when 1 <= rank <=6 *)
  | PawnPromotion of cell*figure (* TODO: figure can't be a king *)

type move = move_kind * mm_postfix

let move_equal m1 m2 = (m1=m2)

let string_of_postfix = function
  | Some `Check -> "+"
  | Some `Checkmate -> "#"
  | None -> ""

let string_of_move: move -> string = fun (k,pf) ->
  let pfs = string_of_postfix pf in
  let add_hint b = function
    | Some (`File file) -> Buffer.add_char b (char_of_file file)
    | Some (`Rank file) -> Buffer.add_char b (char_of_rank file)
    | None -> ()
  in
  with_buffer @@ fun b ->
  let () = match k with
    | PawnPromotion (cell,figure) -> Buffer.add_string b @@
      sprintf "%s=%c" (string_of_cell cell) (char_of_figure figure)
    | PawnTakesPromotion (file,cell,figure) -> Buffer.add_string b @@
      sprintf "%cx%s=%c" (char_of_file file) (string_of_cell cell) (char_of_figure figure)
    | PawnMoves cell ->
      Buffer.add_string b (string_of_cell cell)
    | PawnTakes (file,cell) -> Buffer.add_string b @@
      sprintf "%cx%s%s" (char_of_file file) (string_of_cell cell) pfs
    | FigureMoves (fig,cell,hint,takes) ->
      Buffer.add_char b (char_of_figure fig);
      add_hint b hint;
      if takes then Buffer.add_char b 'x';
      Buffer.add_string b (string_of_cell cell)
    | CastleKingSide -> Buffer.add_string b (sprintf "O-O%s" pfs)
    | CastleQueenSide -> Buffer.add_string b (sprintf "O-O-O%s" pfs)
  in
  Buffer.add_string b pfs


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


let move_of_string move =
  { move; pre_ann=None; post_ann=None; variants=[]; next=`Result NoResult; nags=[] }

let move_tree_to_string root =
  let b = Buffer.create 20 in
  let add_string = Buffer.add_string b in
  let ann = function
    | None   -> ()
    | Some x -> (add_string "{ "; add_string x; add_string " }")
  in
  let rec inner : tree -> unit = fun {move; nags; pre_ann; post_ann; variants; next } ->
      ann pre_ann;
      add_string (string_of_move move);
      add_string " ";
      ann post_ann;
      List.iter (fun root -> add_string "( ";
	let () = match root with
	| `NullMoves xs ->
	  List.iter (fun (text, move) -> ann text; add_string (string_of_move move)) xs
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
  type cell = file*rank with sexp
  type celli = int*int (* [0..7] * [0..7] *)
  type board_cell_content = (color*figure) option with sexp
  type t = board_cell_content array array with sexp

  let figure_of_char = function
    | 'K' -> King   | 'Q' -> Queen  | 'R' -> Rook
    | 'B' -> Bishop | 'N' -> Knight | _ -> failwith "Wrong argument of figure_of_char"

  let celli_of_cell (v,h) =
    let x = match v with
      | VA -> 0 | VB -> 1 | VC -> 2 | VD->3 | VE-> 4 |VF->5 |VG->6 |VH->7 in
    let y = match h with
      | H1 -> 0 | H2 -> 1 | H3 ->2 | H4->3 |H5-> 4| H6->5|H7->6|H8->7 in
    (x,y)

  let cell_of_celli (x,y) =
    let v = match x with
      | 0 -> VA | 1 -> VB|2->VC|3->VD|4->VE|5->VF|6->VG|7->VH
      | _ -> failwith "Bad argument of cell_of_celli"
    in
    let h = match y with
      | 0 -> H1 | 1 -> H2|2->H3|3->H4|4->H5|5->H6|6->H7|7->H8
      | _ -> failwith "Bad argument of cell_of_celli"
    in
    (v,h)

  let string_of_celli (v,h) =
    let ans = "__" in
    ans.[0] <- Char.chr (Char.code 'a' + v - 1);
    ans.[1] <- Char.chr (Char.code '1' + h - 1);
    ans

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

  let next_color = function White -> Black | Black -> White

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

  let horizontal_of_int x = cell_of_celli (0,x) |> snd
  let vertical_of_int x = cell_of_celli (x,0) |> fst

  let color_of_cell (v,h) =
    let v1 = int_of_vertical v in
    let h1 = int_of_horizontal h in
    if (v1+h1) mod 2 = 0 then Black else White

  let cell_of_string s =
    assert (String.length s = 2);
    let v = match s.[0] with
      | 'a' | 'A' -> VA
      | 'b' | 'B' -> VB
      | 'c' | 'C' -> VC
      | 'd' | 'D' -> VD
      | 'e' | 'E' -> VE
      | 'f' | 'F' -> VF
      | 'g' | 'G' -> VG
      | 'h' | 'H' -> VH
      | _ -> failwith "Can't parse vertical" in
    let h = match s.[1] with
      | '1' -> H1 | '2' -> H2 | '3' -> H3 | '4' -> H4 | '5' -> H5 | '6' -> H6 | '7' -> H7 | '8' -> H8
      | _ -> failwith "Can't parse horizontal" in
    (v,h)

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
  let empty_celli b (x,y) = (b.(x).(y) = None)

  let set_cell_value b (v,h) data = b.(int_of_vertical v).(int_of_horizontal h) <- data

  let empty_cell_unsafe b cell =
    match b.(Char.code cell.[0] - Char.code 'a').(Char.code cell.[1] - Char.code '1') with
    | Some x -> false
    | None -> true

  let find_cells ~cellcond figcond b =
    let ans = ref [] in
    for i=0 to 7 do
    for j=0 to 7 do
      let s = "__" in
      s.[0] <- Char.chr (Char.code 'a' + i);
      s.[1] <- Char.chr (Char.code '1' + j);
      let (v,h) = cell_of_string s in
      if cellcond (v,h) && figcond b.(i).(j) then ans := b.(i).(j) :: !ans;
    done;
    done;
    !ans

  let get_possible_kings : color -> cell -> t -> cell list = fun who_moves (v,h) b ->
    let genf (x,y) = fun (a,b) -> (a+x,b+y) in
    let fs = List.map genf [(1,1);(1,0);(1,-1); (0,-1);(0,1); (-1,-1);(-1,0);(-1,1)] in
    let (i1,j1) = celli_of_cell (v,h) in
    let inboard (x,y) = (x>=0)&&(y>=0)&&(x<=7)&&(y<=7) in
    let sq8 = List.filter inboard @@ List.map (fun f -> f (i1,j1)) fs in
    let sq8 = List.filter (fun (x,y) -> b.(x).(y) = Some (who_moves, King)) sq8 in
    List.map cell_of_celli sq8

  let get_possible_knights : color -> cell -> t -> cell list = fun who_moves (v,h) b ->
    let genf (x,y) = fun (a,b) -> (a+x,b+y) in
    let fs = List.map genf [(1,2);(2,1);(2,-1);(1,-2);(-1,-2);(-2,-1);(-2,1);(-1,2)] in
    let (i1,j1) = celli_of_cell (v,h) in
    let inboard (x,y) = (x>=0)&&(y>=0)&&(x<=7)&&(y<=7) in
    let sq8 = List.filter inboard @@ List.map (fun f -> f (i1,j1)) fs in
    let sq8 = List.filter (fun (x,y) -> b.(x).(y) = Some (who_moves, Knight)) sq8 in
    List.map (cell_of_celli) sq8

  let get_possible_queens : color -> cell -> t -> cell list = fun who_moves (v,h) b ->
    let celli = celli_of_cell (v,h) in
    let ans = ref [] in
    let go_tl (x,y) = (x-1,y+1) in
    let go_bl (x,y) = (x-1,y-1) in
    let go_tr (x,y) = (x+1,y+1) in
    let go_br (x,y) = (x+1,y-1) in
    let go_l  (x,y) = (x-1,y) in
    let go_r  (x,y) = (x+1,y) in
    let go_t  (x,y) = (x,y+1) in
    let go_b  (x,y) = (x,y-1) in

    let loop_gen go_f =
      let rec loopTL ((x,y) as loc) =
        if x<0 || y<0 || x>7 || y>7 then () else
        if empty_celli b loc then loopTL (go_f loc) else
        if b.(x).(y) = Some (who_moves, Queen) then
          (ans:=cell_of_celli loc :: !ans; loopTL (go_f loc) )
        else ()
      in
      loopTL
    in
    loop_gen go_tl (go_tl celli);
    loop_gen go_tr (go_tr celli);
    loop_gen go_bl (go_bl celli);
    loop_gen go_br (go_br celli);
    loop_gen go_l  (go_l  celli);
    loop_gen go_r  (go_r  celli);
    loop_gen go_t  (go_t  celli);
    loop_gen go_b  (go_b  celli);
    !ans

  let get_possible_bishops : color -> cell -> t -> cell list = fun who_moves (v,h) b ->
    let celli = celli_of_cell (v,h) in
    let ans = ref [] in
    let go_tl (x,y) = (x-1,y+1) in
    let go_bl (x,y) = (x-1,y-1) in
    let go_tr (x,y) = (x+1,y+1) in
    let go_br (x,y) = (x+1,y-1) in
    let loop_gen go_f =
      let rec loopTL ((x,y) as loc) =
        if x<0 || y<0 || x>7 || y>7 then () else
        if empty_celli b loc then loopTL (go_f loc) else
        if b.(x).(y) = Some (who_moves, Bishop) then
          (ans:=cell_of_celli loc :: !ans; loopTL (go_f loc) )
        else ()
      in
      loopTL
    in
    loop_gen go_tl (go_tl celli);
    loop_gen go_bl (go_bl celli);
    loop_gen go_tr (go_tr celli);
    loop_gen go_br (go_br celli);
    !ans

  let get_possible_rooks : color -> cell -> t -> cell list = fun who_moves from b ->
    let ((i1,j1) as celli) = celli_of_cell from in
    let ans = ref [] in
    let go_l (x,y) = (x-1,y) in
    let go_r (x,y) = (x+1,y) in
    let go_t (x,y) = (x,y+1) in
    let go_b (x,y) = (x,y-1) in
    let loop_gen go_f =
      let rec loop ((x,y) as loc) =
        if x<0 || y<0 || x>7 || y>7 then () else
        if empty_celli b loc then loop (go_f loc) else
        if b.(x).(y) = Some (who_moves, Rook) then
          (ans:=cell_of_celli loc :: !ans; loop (go_f loc) )
        else ()
      in
      loop
    in
    loop_gen go_l (go_l celli);
    loop_gen go_r (go_r celli);
    loop_gen go_t (go_t celli);
    loop_gen go_b (go_b celli);
    !ans

  let wrap_get_possible = function
    | Queen -> get_possible_queens
    | Bishop -> get_possible_bishops
    | Rook   -> get_possible_rooks
    | King   -> get_possible_kings
    | Knight -> get_possible_knights
    | Pawn   -> failwith  "Bad argument of wrap_get_possible"

  let create () =
    let b = Array.init 8 (fun _ -> Array.init 8 (fun _ -> None)) in
    for j=0 to 7 do
      b.(j).(1) <- Some (White,Pawn);
      b.(j).(6) <- Some (Black,Pawn)
    done;
    List.iter (fun (color, horiz) ->
        List.iter (fun v -> set_cell_value b (v,horiz) (Some (color,Rook)) )   [ VA; VH ];
        List.iter (fun v -> set_cell_value b (v,horiz) (Some (color,Knight)) ) [ VB; VG ];
        List.iter (fun v -> set_cell_value b (v,horiz) (Some (color,Bishop)) ) [ VC; VF ];
        set_cell_value b (VD,horiz) (Some (color,Queen));
        set_cell_value b (VE,horiz) (Some (color,King))
      ) [ (White,H1); (Black, H8) ];
    b

  let is_vert c = Char.(code c >= code 'a' && code c <= code 'h')
  let is_horiz c = Char.(code c >= code '1' && code c <= code '8')
  let inverse_color = function White -> Black | Black -> White

  (* for e5 should return e4 *)
  let down_cell_exn (v,h) =
    let _h = int_of_horizontal h in
    if _h=0 then failwith "Bad argument of down_cell_exn";
    (v, horizontal_of_int (_h-1))

  let up_cell_exn (v,h) =
    let _h = int_of_horizontal h in
    if _h=7 then failwith "Bad argument of up_cell_exn";
    (v, horizontal_of_int (_h+1))

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

  let can_castle_ks _board _color = true
  let can_castle_qs _ _ = true

  let make_move: move -> (color * t) -> (color * t) option = fun (move,pfx) (side_color, board) ->
    printf "make_move %s %s, current board:\n%s\n%!"
      (string_of_color side_color) (string_of_move (move,pfx)) (to_string board);
    let icolor = inverse_color side_color in
    let b = copy board in
    match move with
    | CastleKingSide when side_color=White && can_castle_ks b White ->
      assert (get_cell_value b (VE,H1) = Some (side_color, King));
      assert (get_cell_value b (VF,H1) = None);
      assert (get_cell_value b (VG,H1) = None);
      assert (get_cell_value b (VH,H1) = Some (side_color, Rook));
      set_cell_value b (VE,H1) None;
      set_cell_value b (VF,H1) (Some (White,Rook));
      set_cell_value b (VG,H1) (Some (White,King));
      set_cell_value b (VH,H1) None;
      Some (icolor, b)
    | CastleKingSide when side_color=Black && can_castle_ks b Black ->
      assert (get_cell_value b (VE,H8) = Some (side_color, King));
      assert (get_cell_value b (VF,H8) = None);
      assert (get_cell_value b (VG,H8) = None);
      assert (get_cell_value b (VH,H8) = Some (side_color, Rook));
      set_cell_value b (VE,H8) None;
      set_cell_value b (VF,H8) (Some (Black,Rook));
      set_cell_value b (VG,H8) (Some (Black,King));
      set_cell_value b (VH,H8) None;
      Some (icolor, b)
    | CastleKingSide -> None
    | CastleQueenSide when side_color=Black && can_castle_ks b Black ->
      assert (get_cell_value b (VE,H8) = Some (side_color, King));
      assert (get_cell_value b (VD,H8) = None);
      assert (get_cell_value b (VC,H8) = None);
      assert (get_cell_value b (VB,H8) = None);
      assert (get_cell_value b (VA,H8) = Some (side_color, Rook));
      set_cell_value b (VE,H8) None;
      set_cell_value b (VD,H8) (Some (Black,Rook));
      set_cell_value b (VC,H8) (Some (Black,King));
      set_cell_value b (VB,H8) None;
      set_cell_value b (VA,H8) None;
      Some (icolor, b)
    | CastleQueenSide when side_color=White && can_castle_ks b White ->
      assert (get_cell_value b (VE,H1) = Some (side_color, King));
      assert (get_cell_value b (VD,H1) = None);
      assert (get_cell_value b (VC,H1) = None);
      assert (get_cell_value b (VB,H1) = None);
      assert (get_cell_value b (VA,H1) = Some (side_color, Rook));
      set_cell_value b (VE,H1) None;
      set_cell_value b (VD,H1) (Some (White,Rook));
      set_cell_value b (VC,H1) (Some (White,King));
      set_cell_value b (VB,H1) None;
      set_cell_value b (VA,H1) None;
      Some (icolor, b)
    | CastleQueenSide  -> None
    | FigureMoves (fig, dest, mm_fig_hint, mm_takes) ->
      let xs = wrap_get_possible fig side_color dest b in
      printf "get_possible %c's: " (char_of_figure fig);
      List.iter (fun celli -> printf "%s " (string_of_cell celli)) xs;
      printf "\n%!";
      let from = match xs, mm_fig_hint with
        | [x],_ -> x
        | xs,Some (`File v) ->
          let ys = List.filter (fun (_v,_) -> _v=v) xs in
          if List.length ys <> 1 then failwith "Can't remove ambiguities"
          else List.hd ys
        | xs,Some (`Rank h) ->
          let ys = List.filter (fun (_,_h) -> _h=h) xs in
          if List.length ys <> 1 then failwith "Can't remove ambiguities"
          else List.hd ys
        | xs,None -> failwith "XYZ"
      in
      set_cell_value b dest (get_cell_value b from);
      set_cell_value b from None;
      Some (icolor,b)

    | PawnTakes (f1,(f2,_)) when not (near_files f1 f2) ->
      failwith "When pawn takes lines should be near";
    | PawnTakes (file,cell) when side_color=White ->
      let (_,down_rank) = down_cell_exn cell in
      let from = (file,down_rank) in
      (* TODO: implement en passant *)
      if (get_cell_value b from = Some(side_color,Pawn)) && (get_cell_value b cell <> None) then (
        set_cell_value b from None;
        set_cell_value b cell (Some (side_color, Pawn));
        Some (icolor, b)
      ) else None
    | PawnTakes (file,cell) when side_color=Black ->
      let (_,up_rank) = up_cell_exn cell in
      let from = (file,up_rank) in
      (* TODO: implement en passant *)
      if (get_cell_value b from = Some(side_color,Pawn)) && (get_cell_value b cell <> None) then (
        set_cell_value b from None;
        set_cell_value b cell (Some (side_color, Pawn));
        Some (icolor, b)
      ) else None

    | PawnTakes (file,cell) -> None
    | PawnTakesPromotion (file,cell,figure) -> None
    | PawnMoves (_,h) when h=H1 || h=H8 -> None
    | PawnMoves (_,h) when h=H2 && side_color=White -> None
    | PawnMoves (_,h) when h=H7 && side_color=Black -> None
    | PawnMoves (v,H4) when side_color=White ->
      let e2 = (v,H2) in
      let e3 = (v,H3) in
      let e4 = (v,H4) in
      if empty_cell b e3 && (get_cell_value b e2 = Some (White,Pawn)) then begin
        (* Move from starting position *)
        set_cell_value b e2 None;
        set_cell_value b e4 (Some(White,Pawn));
        Some (inverse_color side_color, b)
      end else if empty_cell b e2 && (get_cell_value b e3 = Some (White,Pawn)) then begin
        set_cell_value b e3 None;
        set_cell_value b e4 (Some(White,Pawn));
        Some (inverse_color side_color, b)
      end else failwith (sprintf "Can't make pawn move '%s'" (string_of_move (move,pfx)) )
    | PawnMoves (v,H5) when side_color=Black ->
      let e2 = (v,H7) in
      let e3 = (v,H6) in
      let e4 = (v,H5) in
      if empty_cell b e3 && (get_cell_value b e2 = Some (side_color,Pawn)) then begin
        (* Move from starting position *)
        set_cell_value b e2 None;
        set_cell_value b e4 (Some(side_color,Pawn));
        Some (inverse_color side_color, b)
      end else if (get_cell_value b e3 = Some (side_color,Pawn)) then begin
        set_cell_value b e3 None;
        set_cell_value b e4 (Some(side_color,Pawn));
        Some (inverse_color side_color, b)
      end else failwith (sprintf "Can't make pawn move '%s'" (string_of_move (move,pfx)) )
    | PawnMoves (v,h) when side_color=Black  ->
      printf "Moving black pawn....\n";
      let dest = (v,h) in
      let prev = up_cell_exn dest in
      set_cell_value b dest (get_cell_value b prev);
      set_cell_value b prev None;
      Some (inverse_color side_color, b)
    | PawnMoves dest when side_color=White  ->
      printf "Moving white pawn....\n";
(*      let dest = (v,h) in*)
      let prev = down_cell_exn dest in
      set_cell_value b dest (get_cell_value b prev);
      set_cell_value b prev None;
      Some (inverse_color side_color, b)
    | PawnMoves (v,h)    -> None
    | PawnPromotion (cell,King)   -> None
    | PawnPromotion (cell,figure) when side_color=White   ->
      let from = (fst cell, H7) in
      set_cell_value b from None;
      set_cell_value b cell (Some (side_color, figure));
      Some (icolor, b)
    | PawnPromotion (cell,figure) when side_color=Black ->
      let from = (fst cell, H2) in
      set_cell_value b from None;
      set_cell_value b cell (Some (side_color, figure));
      Some (icolor, b)
    | PawnPromotion (_,_) -> None

 (*
    let b = copy board in
    printf "make_move '%s': cur color: %s, Current board is:\n%s\n" move_str
      (Sexplib.Sexp.to_string_hum @@ sexp_of_color side_color)
      (to_string b);
    (*printf "sexp board %s\n%!" (Sexplib.Sexp.to_string_hum @@ sexp_of_t b);*)
    let is_pawn_move = (String.length move_str=2) && is_vert move_str.[0] && is_horiz move_str.[1] in
    let is_figure_move () =
      let takes = String.index move_str 'x' <> None in
      let figure =
        match move_str.[0] with
        | 'B' -> Bishop | 'N' -> Knight | 'R' -> Rook | 'Q' -> Queen | 'K' -> King
        | _ -> failwith "impossible"
      in
      let index2 = if move_str.[1] = 'x' then 2 else 1 in
      let dest = cell_of_string @@ String.sub move_str index2 2 in
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
      end else if h=H8 then failwith "promotion is not supported" else begin
        printf "Moving pawn....\n";
        let dest = (v,h) in
        let prev = down_cell_exn dest in
        set_cell_value b dest (get_cell_value b prev);
        set_cell_value b prev None;
        Some (inverse_color side_color, b)
        (* failwith "TODO: implement white moves on black's board" *)
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
        end else if  (get_cell_value b e6 = Some (Black,Pawn)) then begin
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
        end else if h=H1 then failwith "Promotion is not supported" else begin
          printf "Moving pawn....\n";
          let dest = (v,h) in
          let prev = up_cell_exn dest in
          set_cell_value b dest (get_cell_value b prev);
          set_cell_value b prev None;
          Some (inverse_color side_color, b)
       end
      | _ -> failwith "impossible"
    end else begin
      let (figure,takes,dest) = is_figure_move () in
      Option.(move_figure b side_color figure takes dest >>= fun x -> Some (next_color side_color,x))
    end*)
end

let validate_game root =
  let (>>=) = Option.(>>=) in
  let rec helper (board: (color * Board.t) option) root =
    let init: (color * Board.t) option  = board >>= Board.make_move root.move in
    let f : (color * Board.t) option -> _ -> (color * Board.t) option =
      fun acc -> function
      | `Continue x -> (helper acc x)   (* TODO *)
      | `NullMoves _ -> acc
    in
    (match root.next with
    | `NullMoves _ -> init
    | `Result _ -> init
    | `Continue root2 -> helper init root2) >>= fun _ -> List.fold_left f init root.variants
  in
  helper (Some (White, Board.create ())) root

