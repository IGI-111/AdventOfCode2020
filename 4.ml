open Core

type batch = (string, string) Hashtbl.t

let batches : batch list =
  let separator = Re2.create_exn "\n\n"
  and field = Re2.create_exn "(\\S+):(\\S+)" in
  In_channel.read_all "4.txt"
  |> Re2.split separator
  |> List.map ~f:(Re2.find_all_exn field)
  |> List.map ~f:(fun fstrings ->
         fstrings
         |> List.map ~f:(fun f ->
                let matches = Re2.find_submatches_exn field f in
                (Option.value_exn matches.(1), Option.value_exn matches.(2)))
         |> Hashtbl.of_alist_exn (module String))

let valid_passport_silver (batch : batch) : bool =
  List.for_all
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    ~f:(Hashtbl.mem batch)

let validate_hgt hgt =
  let height_format = Re2.create_exn "^([0-9]+)(cm|in)$" in
  Re2.matches height_format hgt
  &&
  let matches = Re2.find_submatches_exn height_format hgt in
  let height = Option.value_exn matches.(1) |> int_of_string
  and hunit = Option.value_exn matches.(2) in
  if String.equal "cm" hunit then height >= 150 && height <= 193
  else if String.equal "in" hunit then height >= 59 && height <= 76
  else false

let validate_hcl hcl =
  let hcl_format = Re2.create_exn "^#[0-9a-f]{6}$" in
  Re2.matches hcl_format hcl

let validate_ecl ecl =
  List.mem
    [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
    ecl ~equal:String.equal

let validate_pid pid =
  let pid_format = Re2.create_exn "^[0-9]{9}$" in
  Re2.matches pid_format pid

let validate_byr byr_str =
  let byr_format = Re2.create_exn "^[0-9]{4}$" in
  Re2.matches byr_format byr_str
  &&
  let byr = int_of_string byr_str in
  byr >= 1920 && byr <= 2002

let validate_iyr iyr_str =
  let iyr_format = Re2.create_exn "^[0-9]{4}$" in
  Re2.matches iyr_format iyr_str
  &&
  let iyr = int_of_string iyr_str in
  iyr >= 2010 && iyr <= 2020

let validate_eyr eyr_str =
  let eyr_format = Re2.create_exn "^[0-9]{4}$" in
  Re2.matches eyr_format eyr_str
  &&
  let eyr = int_of_string eyr_str in
  eyr >= 2020 && eyr <= 2030

let valid_passport_gold (batch : batch) : bool =
  valid_passport_silver batch
  &&
  let byr = Hashtbl.find_exn batch "byr"
  and iyr = Hashtbl.find_exn batch "iyr"
  and eyr = Hashtbl.find_exn batch "eyr"
  and hgt = Hashtbl.find_exn batch "hgt"
  and hcl = Hashtbl.find_exn batch "hcl"
  and ecl = Hashtbl.find_exn batch "ecl"
  and pid = Hashtbl.find_exn batch "pid" in
  validate_byr byr && validate_iyr iyr && validate_eyr eyr && validate_hgt hgt
  && validate_hcl hcl && validate_ecl ecl && validate_pid pid

let print_batch batch =
  Hashtbl.iteri batch ~f:(fun ~key ~data -> printf "'%s' = '%s'\n" key data);
  printf "\n"

let () =
  List.fold batches ~init:(0, 0) ~f:(fun (acc_silver, acc_gold) batch ->
      ( (if valid_passport_silver batch then acc_silver + 1 else acc_silver),
        if valid_passport_gold batch then (
          printf "valid\n";
          print_batch batch;
          acc_gold + 1 )
        else (
          printf "invalid\n";
          print_batch batch;
          acc_gold ) ))
  |> fun (silver, gold) -> printf "%d\n%d\n" silver gold
