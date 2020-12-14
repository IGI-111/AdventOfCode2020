open Core

type ins =
  | North of int
  | South of int
  | East of int
  | West of int
  | Left of int
  | Right of int
  | Forward of int

exception Unknown_ins_type of char

let instructions =
  In_channel.read_lines "12.txt"
  |> List.map ~f:(fun line ->
         line |> String.to_list |> fun chars ->
         let num =
           chars |> List.tl_exn |> List.map ~f:Char.to_string |> String.concat
           |> Int.of_string
         in
         match List.hd_exn chars with
         | 'N' -> North num
         | 'S' -> South num
         | 'E' -> East num
         | 'W' -> West num
         | 'L' -> Left num
         | 'R' -> Right num
         | 'F' -> Forward num
         | c -> raise (Unknown_ins_type c))

let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y

exception Invalid_azimuth of int

let path_end_silver (path : ins list) : int * int =
  let rec run_path (x, y) az path =
    if List.is_empty path then (x, y)
    else
      match List.hd_exn path with
      | North n -> run_path (x, y + n) az (List.tl_exn path)
      | South n -> run_path (x, y - n) az (List.tl_exn path)
      | East n -> run_path (x + n, y) az (List.tl_exn path)
      | West n -> run_path (x - n, y) az (List.tl_exn path)
      | Left n -> run_path (x, y) (modulo (az - n) 360) (List.tl_exn path)
      | Right n -> run_path (x, y) (modulo (az + n) 360) (List.tl_exn path)
      | Forward n -> (
          match az with
          | 0 -> run_path (x, y + n) az (List.tl_exn path)
          | 90 -> run_path (x + n, y) az (List.tl_exn path)
          | 180 -> run_path (x, y - n) az (List.tl_exn path)
          | 270 -> run_path (x - n, y) az (List.tl_exn path)
          | n -> raise (Invalid_azimuth n) )
  in
  run_path (0, 0) 90 path

exception Invalid_rotation_angle of int

let path_end_gold (path : ins list) : int * int =
  let rotate (x, y) angle =
    match modulo angle 360 with
    | 0 -> (x, y)
    | 90 -> (y, -x)
    | 180 -> (-x, -y)
    | 270 -> (-y, x)
    | n -> raise (Invalid_rotation_angle n)
  in
  let rec run_path (x, y) (wx, wy) path =
    if List.is_empty path then (x, y)
    else
      match List.hd_exn path with
      | North n -> run_path (x, y) (wx, wy + n) (List.tl_exn path)
      | South n -> run_path (x, y) (wx, wy - n) (List.tl_exn path)
      | East n -> run_path (x, y) (wx + n, wy) (List.tl_exn path)
      | West n -> run_path (x, y) (wx - n, wy) (List.tl_exn path)
      | Left n -> run_path (x, y) (rotate (wx, wy) (-n)) (List.tl_exn path)
      | Right n -> run_path (x, y) (rotate (wx, wy) n) (List.tl_exn path)
      | Forward n ->
          run_path (x + (n * wx), y + (n * wy)) (wx, wy) (List.tl_exn path)
  in
  run_path (0, 0) (10, 1) path

let manhattan_dst (x, y) = Int.abs x + Int.abs y

let () =
  instructions |> path_end_silver |> manhattan_dst |> printf "%d\n";
  instructions |> path_end_gold |> manhattan_dst |> printf "%d\n"
