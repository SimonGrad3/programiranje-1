type available = { loc : int * int; mutable possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)

type state = { 
  problem : Model.problem; 
  mutable current_grid : int option Model.grid; 
  mutable  available : available list 
  }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state ={ 
  current_grid = Model.copy_grid problem.initial_grid;
  problem ; 
  available = [] 
  }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state




(*---------------------- POMOŽNE FUNKCIJE --------------------------- *)


let poisci_prazno_celico grid = 
  let rec aux grid i j =
    match (i, j) with
    |(8, 9) -> failwith "Očitno sudoku cel izpolnjen"
    |(i, 9) -> aux grid (i + 1) 0
    |(i, j) ->
      match grid.(i).(j)  with
      | None -> (i , j)
      | Some dig -> aux grid i (j + 1)
  in 
  aux grid 0 0


let ugotovi_box (i,j) =
  match (i,j) with
  | (i,j) when ((i <= 2) && (j <= 2)) -> 0
  | (i,j) when ((i <= 2) && (3 <= j ) && (j <= 5)) -> 1
  | (i,j) when ((i <= 2) && (6 <= j)) -> 2
  | (i,j) when ((3 <= i) && (i <= 5) && (j <= 2)) -> 3
  | (i,j) when ((3 <= i) && (i <= 5) && (3 <= j) && (j <= 5)) -> 4
  | (i,j) when ((3 <= i) && (i <= 5) && (6 <= j)) -> 5
  | (i,j) when ((6 <= i) && (j <= 2)) -> 6
  | (i,j) when ((6 <= i) && (3 <= j) && (j <= 5)) -> 7
  | (i,j) when ((6 <= i) && (6 <= j)) -> 8
  | _ -> failwith "Ta index ne obstaja"

let spremeni_v_int seznam =
  (* Funkcija ki 'a option seznam spremeni v 'a seznam *)
  let rec aux seznam acc =
    match seznam with
    | [] -> acc
    | a :: b when a = None -> aux b acc
    | (Some dig) :: b -> aux b (dig :: acc)
    | _ -> acc 
  in
  aux seznam []
  
(* Pozor tukaj je sez od 1 do 9 razen če smo že kaj zbrisali *)
let preglej (grid : 'a option Model.grid) (i, j) sez : int list = 
  (* Funkcija, ki preveri katere številke so možne v celici (i,j) *)
  let seznam_option = (Array.to_list (Model.get_row grid i)) @ 
  (Array.to_list (Model.get_column grid j)) @ 
  Array.to_list (Model.get_box grid (ugotovi_box (i,j))) in 
  let seznam = spremeni_v_int(seznam_option) in
  let rec preglej_sezname seznam sez acc =
    match sez with
    | [] -> acc 
    | a :: b when (List.exists (fun x -> a = x) seznam) -> preglej_sezname seznam b acc
    | a :: b -> preglej_sezname seznam b (a :: acc)
  in
    preglej_sezname seznam sez []


let rec spremeni_available_v_state state (i , j) list  =
  (* V stateu spremeni pri določenem (i,j) list možnih števil *)
  let rec funk seznam = 
    match seznam with
    | [] -> failwith "available_list je prazen."
    | a :: _ when a.loc = (i,j) -> a
    | _ :: b ->  funk b
    in
    (funk state.available).possible <- list;
  state.available

let dodaj_cifro grid (i,j) cifra =
  let novi_grid = Model.copy_grid grid in
    novi_grid.(i).(j)  <- Some cifra;
    novi_grid

let preveri_available_list state (i,j) list =
  match list with
  | [] -> None
  | a :: b -> Some (
    {problem = state.problem ;
    current_grid = dodaj_cifro (state.current_grid) (i,j) a ;
  available = spremeni_available_v_state state (i,j) b}
  , 
  {problem = state.problem;
    current_grid = state.current_grid;
    available = spremeni_available_v_state state (i,j) b
    })  


let rec poisci_available available_list (i , j) =
  (* V available listu poišče tisti available z loc = (i,j) in vrne njemu pripradjoč list possiblov. *)
  match available_list with
  | [] -> failwith "available_list je prazen."
  | a :: _ when a.loc = (i,j) -> a.possible
  | _ :: b ->  poisci_available b (i , j)


let posodobi_available state =
  (* Posodobi celoten available list *)
  let rec aux acc available =
    match available with 
    | [] -> {state with available = acc}
    | {loc = (i,j); possible = list} :: b -> aux ({loc = (i,j); possible = preglej (state.current_grid) (i,j) list } :: acc) b
  in 
  aux [] state.available
      

(* ------------------------ KONEC POMOŽNIH FUNKCIJ-------------------- *)

let zacetni_pregled state =
  (* Ta funkcija dodela available, list, v state, da je poln.*)
  let y = Array.length (state.current_grid)  in
  let x = Array.length ( state.current_grid.(0) ) in
  let rec prevrti state (i, j) =  
    let list = preglej state.current_grid (i, j) [1;2;3;4;5;6;7;8;9] in
    state.available <- {loc = (i,j); possible = list} :: state.available;
    match (i, j) with
    | (i, j) when (i = x-1) && (j = y-1) -> state
    | (_, j) when j < ( y - 1 ) -> prevrti state (i, (j + 1))
    | (_, j) -> prevrti state ((i + 1), 0)
    in 
    prevrti state (0, 0)

    
let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  let (i, j) = poisci_prazno_celico (state.current_grid) in
    preveri_available_list 
      state 
      (i, j) 
      (preglej (state.current_grid) (i, j) ( poisci_available (state.available) (i,j) )) 
     
     
(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  let state = posodobi_available state in
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> zacetni_pregled |> solve_state
