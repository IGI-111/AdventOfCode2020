open Core

type policy = {
  low_bound : int;
  high_bound : int;
  letter : char;
  password : string;
}

let policy_regex = Re2.create_exn "(\\d+)-(\\d+) (\\w): (\\w+)"

let parse_regex (str : string) : policy =
  let matches = Re2.find_submatches_exn policy_regex str in
  {
    low_bound = Option.value_exn matches.(1) |> int_of_string;
    high_bound = Option.value_exn matches.(2) |> int_of_string;
    letter = (Option.value_exn matches.(3)).[0];
    password = Option.value_exn matches.(4);
  }

let entries = In_channel.read_lines "2.txt"

let count_char (str : string) (to_count : char) : int =
  String.fold ~init:0
    ~f:(fun acc c -> if Char.equal c to_count then acc + 1 else acc)
    str

let is_valid_silver (p : policy) : bool =
  let actual = count_char p.password p.letter in
  actual <= p.high_bound && actual >= p.low_bound

let is_valid_gold (p : policy) : bool =
  let a = Char.equal p.password.[p.low_bound - 1] p.letter
  and b = Char.equal p.password.[p.high_bound - 1] p.letter in
  ((not a) && b) || (a && not b)

let () =
  entries |> List.map ~f:parse_regex
  |> List.fold ~init:(0, 0) ~f:(fun (acc_silver, acc_gold) p ->
         ( (if is_valid_silver p then acc_silver + 1 else acc_silver),
           if is_valid_gold p then acc_gold + 1 else acc_gold ))
  |> fun (silver, gold) -> printf "%d\n%d\n" silver gold
