open Core.Std

type entry =
  { mutable laps : float list
  }

let data : entry String.Table.t = String.Table.create () ~size:10 ;;

let record ~label ~action : 'a =
  let entry = match Hashtbl.find data label with
  | None   ->
      let entry = { laps = [] } in
      Hashtbl.set data ~key:label ~data:entry; entry
  | Some v -> v
  in
  let base = Unix.gettimeofday () in
  let ret = action () in
  entry.laps <- (Unix.gettimeofday () -. base) :: entry.laps;
  ret

let report (_:unit) : (string * (int * float)) list =
  let sum = List.fold_left ~f:(+.) ~init:0.0 in
  Hashtbl.fold
    ~f:(fun ~key:l ~data:e ans -> (l, (List.length e.laps, sum e.laps)) :: ans)
    ~init:[]
    data
