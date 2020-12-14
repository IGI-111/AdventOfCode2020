open Core

let adapters =
  In_channel.read_lines "10.txt"
  |> List.map ~f:int_of_string
  |> List.sort ~compare:Int.compare

let longest_joltage_chain adapters =
  let rec joltages_rec root adpt : int list =
    let start : int = List.hd_exn root and next_adpt = List.hd adpt in
    match next_adpt with
    | None -> root
    | Some next ->
        if next <= start + 3 then
          joltages_rec (next :: root) (List.tl adpt |> Option.value ~default:[])
        else root
  in
  let chain = joltages_rec [ 0 ] adapters in
  (List.hd_exn chain + 3) :: chain

let differences chain =
  List.fold chain
    ~init:(0, (0, 0, 0))
    ~f:(fun (prev, (diff1, diff2, diff3)) v ->
      ( v,
        ( (if prev - v = 1 then diff1 + 1 else diff1),
          (if prev - v = 2 then diff2 + 1 else diff2),
          if prev - v = 3 then diff3 + 1 else diff3 ) ))
  |> fun (_, diffs) -> diffs

let available_joltage_chains_count adapters =
  let adpts = Hash_set.of_list (module Int) adapters
  and memo = Hashtbl.create (module Int) in
  let rec joltages_rec start =
    if Hashtbl.mem memo start then Hashtbl.find_exn memo start
    else
      let res =
        [ 1; 2; 3 ] |> List.filter ~f:(fun x -> Hash_set.mem adpts (start + x))
        |> fun tgts ->
        if List.is_empty tgts then 1
        else
          List.fold tgts ~init:0 ~f:(fun acc incr ->
              acc + joltages_rec (start + incr))
      in
      Hashtbl.set memo ~key:start ~data:res;
      res
  in

  joltages_rec 0

let () =
  differences chain |> fun (one, two, three) ->
  one * three |> printf "%d\n";
  available_joltage_chains_count adapters |> printf "%d\n"
