let read_file f =
  let ic = open_in f in
  let lines = ref [] in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    close_in ic;
    List.rev !lines;;

(* row 0 is top of map; col 0 is left-most col of map *)
type pos = { row : int; col : int}

let traverse (map : string list) slope_right slope_down =
  let rec traverse' pos =
    if pos.row >= List.length map
      then []
      else begin
        let row = List.nth map pos.row in
        let col =
          if pos.col >= String.length row
            then pos.col mod String.length row
            else pos.col
          in 
        let c = String.get row col in
        let new_pos = { row = pos.row + slope_down; col = pos.col + slope_right } in
        c :: traverse' new_pos
      end
    in traverse' { row = 0; col = 0}

let solve (slopes : (int * int) list) : int =
  let map = read_file "input" in
  List.map (fun (slope_right, slope_down) -> 
    let path = traverse map slope_right slope_down in
    let n_trees = List.filter (fun c -> c == '#') path |> List.length in
    n_trees
  ) slopes |> List.fold_left (fun a b -> a * b) 1

let part1 = solve [(3, 1)]

let part2 = solve [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]

let () =
  print_int part1;;
  print_newline ();;
  print_int part2
