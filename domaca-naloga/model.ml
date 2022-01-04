(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind) 

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  
  let rec zajami (grid : 'a grid) (a : int) (b : int) (c : int) (d : int) =
    let rec prevrti_po_stolpcih grid (acc : 'a list) a b c d =
      match c with
      | c when c <= d -> prevrti_po_stolpcih grid ( grid.(a).(c) :: acc) a b (c + 1) d
      | c -> acc
    in 
      let rec prevrti_po_vrsticah grid (acc : 'a list) a b c d =
        match a with
        | a when a <= b -> prevrti_po_vrsticah grid (prevrti_po_stolpcih grid [] a b c d @ acc) (a+1) b c d 
        | a -> acc
      in 
        Array.of_list (List.rev (prevrti_po_vrsticah grid [] a b c d))
  in
    
  let funkcija (a, b, c, d) = zajami grid a b c d in
  
    match box_ind with
    | 0 -> funkcija (0,2,0,2)
    | 1 -> funkcija (0,2,3,5)
    | 2 -> funkcija (0,2,6,8)
    | 3 -> funkcija (3,5,0,2)
    | 4 -> funkcija (3,5,3,5)
    | 5 -> funkcija (3,5,6,8)
    | 6 -> funkcija (6,8,0,2)
    | 7 -> funkcija (6,8,3,5)
    | _ -> funkcija (6,8,6,8)


let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun vrstica -> Array.map f (get_row grid vrstica))  

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit =
  let string_of_option x =
    match x with
    | None -> " "
    | Some i -> string_of_int i
  in
  print_grid string_of_option problem


let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let preveri_ujemanje problem solution i j =
  match problem.initial_grid.(i).(j) with
  | None -> true
  | Some dig when (dig = solution.(i).(j))  -> true
  | _ -> false

let is_valid_solution (problem : problem) (solution : solution) = 
  let seznam = (rows solution) @ (columns solution) @ (boxes solution) in
    let rec preveri_stevila seznam = 
      match seznam with
      | [] -> true
      | a :: b when (List.sort (Int.compare) (Array.to_list a)) = [1;2;3;4;5;6;7;8;9] -> 
        preveri_stevila b 
      | a :: b -> false
    in
      let rec preveri_ujemanje_aux problem solution velikost i j  =
        match (i, j) with
        | (0, 0) -> preveri_ujemanje problem solution i j
        | (0, b) -> 
          (preveri_ujemanje problem solution i j) && (preveri_ujemanje_aux problem solution velikost velikost (j - 1))
        | (a, _) -> (preveri_ujemanje problem solution i j) && (preveri_ujemanje_aux problem solution velikost (i-1) j)
      in ((preveri_ujemanje_aux problem solution 8 8 8) && (preveri_stevila seznam))





