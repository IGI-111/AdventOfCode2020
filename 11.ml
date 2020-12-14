open Core

module IntPair = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare

    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t

    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
  end

  include T
  include Comparable.Make (T)
end

exception Unknown_seat_type

type seats = {
  width : int;
  height : int;
  data : (int * int, bool, IntPair.comparator_witness) Map.t;
}

let seats : seats =
  let lines = In_channel.read_lines "11.txt" |> List.map ~f:String.to_list in
  lines
  |> List.mapi ~f:(fun y line ->
         line
         |> List.foldi ~init:[] ~f:(fun x acc c ->
                match c with
                | 'L' -> ((x, y), false) :: acc
                | '.' -> acc
                | _ -> raise Unknown_seat_type))
  |> List.concat
  |> Map.of_alist_exn (module IntPair)
  |> fun data ->
  { width = List.length (List.hd_exn lines); height = List.length lines; data }

let iterate_silver (s : seats) : seats =
  let adj_occ (x, y) s =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
    |> List.map ~f:(fun (ox, oy) -> (ox + x, oy + y))
    |> List.map ~f:(fun pos -> Map.mem s.data pos && Map.find_exn s.data pos)
    |> List.filter ~f:Fn.id |> List.length
  in
  {
    width = s.width;
    height = s.height;
    data =
      Map.mapi s.data ~f:(fun ~key:pos ~data:occ ->
          if (not occ) && adj_occ pos s = 0 then true
          else if occ && adj_occ pos s >= 4 then false
          else occ);
  }

let iterate_gold (s : seats) : seats =
  let rec raycast (x, y) (ox, oy) s =
    if x >= s.width || x < 0 || y >= s.height || y < 0 then None
    else if Map.mem s.data (x, y) then Map.find s.data (x, y)
    else raycast (x + ox, y + oy) (ox, oy) s
  in

  let adj_occ (x, y) s =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
    |> List.map ~f:(fun (ox, oy) -> raycast (x + ox, y + oy) (ox, oy) s)
    |> List.map ~f:(function Some v -> v | None -> false)
    |> List.filter ~f:Fun.id |> List.length
  in
  {
    width = s.width;
    height = s.height;
    data =
      Map.mapi s.data ~f:(fun ~key:pos ~data:occ ->
          if (not occ) && adj_occ pos s = 0 then true
          else if occ && adj_occ pos s >= 5 then false
          else occ);
  }

let rec stable_state (iterate : seats -> seats) (init : seats) : seats =
  let next = iterate init in
  if Map.equal Bool.equal next.data init.data then init
  else stable_state iterate next

let count_occ s =
  s.data |> Map.data
  |> List.fold ~init:0 ~f:(fun acc occ -> if occ then acc + 1 else acc)

let () =
  seats |> stable_state iterate_silver |> count_occ |> printf "%d\n";
  seats |> stable_state iterate_gold |> count_occ |> printf "%d\n"
