open Core

module IntPair = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare

    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t

    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
end

let seats = In_channel.read_lines "5.txt"

let seat_row seat =
  seat |> String.to_list |> List.sub ~pos:0 ~len:7
  |> List.fold ~init:(0, 127) ~f:(fun (low, up) c ->
         let mid = (low + up) / 2 in
         if Char.equal c 'F' then (low, mid) else (mid, up))
  |> Tuple2.get2

let seat_column seat =
  seat |> String.to_list |> List.sub ~pos:7 ~len:3
  |> List.fold ~init:(0, 7) ~f:(fun (low, up) c ->
         let mid = (low + up) / 2 in
         if Char.equal c 'L' then (low, mid) else (mid, up))
  |> Tuple2.get2

let seat_id row col = (row * 8) + col

let () =
  let max_id =
    Option.value_exn
      ( seats
      |> List.map ~f:(fun s -> seat_id (seat_row s) (seat_column s))
      |> List.max_elt ~compare:Int.compare )
  in
  printf "%d\n" max_id;
  let seat_coords =
    seats |> List.map ~f:(fun s -> (seat_row s, seat_column s))
  in
  let seats_taken = Hash_set.of_list (module IntPair) seat_coords in
  List.range 0 8
  |> List.iter ~f:(fun col ->
         List.range 0 128
         |> List.iter ~f:(fun row ->
                let current = (row, col)
                and prev = (row - 1, col)
                and next = (row + 1, col) in
                if
                  Hash_set.mem seats_taken prev
                  && Hash_set.mem seats_taken next
                  && not (Hash_set.mem seats_taken current)
                then printf "%d\n" (seat_id row col)))
