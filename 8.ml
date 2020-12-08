open Core

type instruction = Nop of int | Acc of int | Jmp of int

exception Unknown_instruction of string

let parse_instruction (ins_str : string) : instruction =
  let ins_format = Re2.create_exn "^(nop|jmp|acc) ((\\+|-)\\d+)$" in
  let matches = Re2.find_submatches_exn ins_format ins_str in
  let num = Option.value_exn matches.(2) |> int_of_string in
  match Option.value_exn matches.(1) with
  | "nop" -> Nop num
  | "acc" -> Acc num
  | "jmp" -> Jmp num
  | s -> raise (Unknown_instruction s)

let instructions : instruction array =
  In_channel.read_lines "8.txt"
  |> List.map ~f:parse_instruction
  |> Array.of_list

let run_until_loop (prog : instruction array) : int =
  let rec run_until_loop_rec current_ins seen_ins acc_val =
    if Set.mem seen_ins current_ins then acc_val
    else
      match prog.(current_ins) with
      | Nop _ ->
          run_until_loop_rec (current_ins + 1)
            (Set.add seen_ins current_ins)
            acc_val
      | Acc v ->
          run_until_loop_rec (current_ins + 1)
            (Set.add seen_ins current_ins)
            (acc_val + v)
      | Jmp v ->
          run_until_loop_rec (current_ins + v)
            (Set.add seen_ins current_ins)
            acc_val
  in
  run_until_loop_rec 0 (Set.empty (module Int)) 0

let terminates (prog : instruction array) : int option =
  let rec terminates_rec current_ins seen_ins acc_val =
    if Set.mem seen_ins current_ins then None
    else if current_ins = Array.length prog - 2 then Some acc_val
    else
      match prog.(current_ins) with
      | Nop _ ->
          terminates_rec (current_ins + 1)
            (Set.add seen_ins current_ins)
            acc_val
      | Acc v ->
          terminates_rec (current_ins + 1)
            (Set.add seen_ins current_ins)
            (acc_val + v)
      | Jmp v ->
          terminates_rec (current_ins + v)
            (Set.add seen_ins current_ins)
            acc_val
  in
  terminates_rec 0 (Set.empty (module Int)) 0

let possible_changed_programs (prog : instruction array) :
    instruction array list =
  Array.foldi prog ~init:[] ~f:(fun i acc ins ->
      match ins with
      | Nop v ->
          let new_prog = Array.copy prog in
          new_prog.(i) <- Jmp v;
          new_prog :: acc
      | Jmp v ->
          let new_prog = Array.copy prog in
          new_prog.(i) <- Nop v;
          new_prog :: acc
      | _ -> acc)

let () =
  run_until_loop instructions |> printf "%d\n";
  possible_changed_programs instructions |> List.find_map ~f:terminates
  |> fun x -> Option.value_exn x |> printf "%d\n"
