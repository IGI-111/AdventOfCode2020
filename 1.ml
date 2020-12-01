open Core

let entries = In_channel.read_lines "1.txt" |> List.map ~f:int_of_string

let pairs lst =
  List.mapi lst ~f:(fun i left ->
      List.drop lst (i + 1) |> List.map ~f:(fun right -> (left, right)))
  |> List.join

let triplets lst =
  List.mapi lst ~f:(fun i left ->
      List.drop lst (i + 1)
      |> pairs
      |> List.map ~f:(fun (middle, right) -> (left, middle, right)))
  |> List.join

let () =
  pairs entries
  |> List.find ~f:(fun (left, right) -> left + right = 2020)
  |> Option.iter ~f:(fun (left, right) -> printf "%d\n" (left * right));
  triplets entries
  |> List.find ~f:(fun (left, middle, right) -> left + middle + right = 2020)
  |> Option.iter ~f:(fun (left, middle, right) ->
         printf "%d\n" (left * middle * right))
