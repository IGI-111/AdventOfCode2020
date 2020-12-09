open Core

let input : int list =
  In_channel.read_lines "9.txt" |> List.map ~f:int_of_string

let possible_sums nums =
  List.foldi nums
    ~init:(Set.empty (module Int))
    ~f:(fun i acc a ->
      List.drop nums (i + 1)
      |> List.fold
           ~init:(Set.empty (module Int))
           ~f:(fun acc b -> Set.add acc (a + b))
      |> Set.union acc)

let validity (len : int) (nums : int list) : (int * bool) list =
  List.append
    (List.take nums len |> List.map ~f:(fun n -> (n, true)))
    ( List.drop nums len
    |> List.mapi ~f:(fun i n ->
           let preamble = List.take (List.drop nums i) len in
           let is_valid =
             preamble |> possible_sums |> fun set -> Set.mem set n
           in
           (n, is_valid)) )

let contiguous_sum (target : int) (nums : int list) : int list =
  List.find_mapi_exn nums ~f:(fun i _ ->
      let sum, lst =
        List.fold_until (List.drop nums i) ~init:(0, [])
          ~f:(fun (sum, lst) x ->
            let new_sum = sum + x and new_lst = x :: lst in
            if new_sum > target then Stop (sum, lst)
            else Continue (new_sum, new_lst))
          ~finish:Fun.id
      in
      if sum = target then Some lst else None)

let encryption_weakness (range : int list) : int =
  let min = Option.value_exn (List.min_elt ~compare:Int.compare range)
  and max = Option.value_exn (List.max_elt ~compare:Int.compare range) in
  min + max

let () =
  let invalid_number =
    validity 25 input
    |> List.filter ~f:(fun (_, is_valid) -> not is_valid)
    |> List.map ~f:Tuple2.get1 |> List.hd_exn
  in
  printf "%d\n" invalid_number;
  contiguous_sum invalid_number input |> encryption_weakness |> printf "%d\n"
