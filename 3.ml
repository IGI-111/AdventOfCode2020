open Core

type map = { width : int; height : int; trees : (int * int) list }

let parseTrees (lines : string list) : (int * int) list =
  lines
  |> List.mapi ~f:(fun y line ->
         line |> String.to_list
         |> List.mapi ~f:(fun x c ->
                if Char.equal c '#' then [ (x, y) ] else [])
         |> List.join)
  |> List.join

let parseMap (lines : string list) : map =
  {
    width = lines |> List.hd_exn |> String.length;
    height = List.length lines;
    trees = parseTrees lines;
  }

let generate_path (x_offset : int) (y_offset : int) (height : int) :
    (int * int) list =
  let rec path_rec height (cur_x, cur_y) =
    if cur_y + y_offset > height - 1 then []
    else
      List.append
        [ (cur_x + x_offset, cur_y + y_offset) ]
        (path_rec height (cur_x + x_offset, cur_y + y_offset))
  in
  path_rec height (0, 0)

let map_path (map : map) (path : (int * int) list) : (int * int) list =
  List.map path ~f:(fun (x, y) -> (x mod map.width, y mod map.height))

let trees_in_path (map : map) (path : (int * int) list) =
  path
  |> List.fold ~init:0 ~f:(fun acc coor ->
         if
           List.mem map.trees coor ~equal:(fun (x1, y1) (x2, y2) ->
               x1 = x2 && y1 = y2)
         then acc + 1
         else acc)

let entry_map = In_channel.read_lines "3.txt" |> parseMap

let () =
  generate_path 3 1 entry_map.height
  |> map_path entry_map |> trees_in_path entry_map |> printf "%d\n";
  [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
  |> List.map ~f:(fun (x_offset, y_offset) ->
         generate_path x_offset y_offset entry_map.height
         |> map_path entry_map |> trees_in_path entry_map)
  |> List.fold ~init:1 ~f:(fun acc count -> acc * count)
  |> printf "%d\n"
