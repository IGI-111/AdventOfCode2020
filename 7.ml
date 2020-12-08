open Core

type rule = (string, int) Hashtbl.t

type rules = (string, rule) Hashtbl.t

let parse_rule (rule_str : string) : string * rule =
  let rule_format = Re2.create_exn "^(\\w+( \\w+)+) bags contain (.*)\\.$" in
  let matches = Re2.find_submatches_exn rule_format rule_str in
  ( Option.value_exn matches.(1),
    let contents_str = Option.value_exn matches.(3) in
    if String.equal contents_str "no other bags" then
      Hashtbl.create (module String)
    else
      let contents_format = Re2.create_exn "(\\d+) (\\w+( \\w+)+) bags?" in
      Re2.find_all_exn contents_format contents_str
      |> List.map ~f:(function c ->
             Re2.find_submatches_exn contents_format c |> fun matches ->
             ( Option.value_exn matches.(2),
               Option.value_exn matches.(1) |> int_of_string ))
      |> Hashtbl.of_alist_exn (module String) )

let rules : rules =
  In_channel.read_lines "7.txt"
  |> List.map ~f:parse_rule
  |> Hashtbl.of_alist_exn (module String)

let rec may_contain bag_type =
  let leafs =
    rules
    |> Hashtbl.filter ~f:(fun rule -> Hashtbl.mem rule bag_type)
    |> Hashtbl.keys
    |> Set.of_list (module String)
  in

  Set.fold leafs
    ~init:(Set.empty (module String))
    ~f:(fun acc leaf ->
      Set.singleton (module String) leaf
      |> Set.union acc
      |> Set.union (may_contain leaf))

let bags_inside_count bag_type =
  let rec bags_inside_count_rec bag_type =
    1
    + ( Hashtbl.find_exn rules bag_type
      |> Hashtbl.fold ~init:0
           ~f:(fun ~key:inner_bag_type ~data:inner_bag_count acc ->
             acc + (inner_bag_count * bags_inside_count_rec inner_bag_type)) )
  in
  bags_inside_count_rec bag_type - 1

let () =
  may_contain "shiny gold" |> Set.length |> printf "%d\n";
  bags_inside_count "shiny gold" |> printf "%d\n"
