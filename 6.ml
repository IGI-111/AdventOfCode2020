open Core

let groups =
  let separator = Re2.create_exn "\n\n" in
  In_channel.read_all "6.txt"
  |> Re2.split separator
  |> List.map ~f:(fun gstr -> gstr |> String.strip |> String.split ~on:'\n')

let unique_answers_in_group group =
  group |> List.map ~f:String.to_list |> List.join
  |> Set.of_list (module Char)
  |> Set.to_list

let common_answers_in_group group =
  group
  |> List.map ~f:(fun answers ->
         answers |> String.to_list |> Set.of_list (module Char))
  |> (function
       | [] -> Set.empty (module Char)
       | head :: tail -> List.fold ~init:head ~f:Set.inter tail)
  |> Set.to_list

let () =
  groups
  |> List.map ~f:(fun group -> group |> unique_answers_in_group |> List.length)
  |> List.sum (module Int) ~f:Fun.id
  |> printf "%d\n";
  groups
  |> List.map ~f:(fun group -> group |> common_answers_in_group |> List.length)
  |> List.sum (module Int) ~f:Fun.id
  |> printf "%d\n"
