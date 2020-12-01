open Core

let entries = In_channel.read_lines "1.txt" |> List.map ~f:int_of_string

let entry_set = Hash_set.of_list (module Int) entries

let remainder value target entry_set =
  if Hash_set.mem entry_set (target - value) then Some (target - value)
  else None

let () =
  Hash_set.find_map entry_set ~f:(fun value ->
      remainder value 2020 entry_set
      |> Option.map ~f:(fun remainder -> (value, remainder)))
  |> Option.iter ~f:(fun (value, remainder) ->
         printf "%d\n" (value * remainder));

  Hash_set.find_map entry_set ~f:(fun value ->
      Hash_set.find_map entry_set ~f:(fun second ->
          remainder second (2020 - value) entry_set
          |> Option.map ~f:(fun remainder -> (value, second, remainder))))
  |> Option.iter ~f:(fun (value, second, remainder) ->
         printf "%d\n" (value * second * remainder))
