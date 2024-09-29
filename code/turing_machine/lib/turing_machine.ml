type tape = {
  left_tape : string option list;
  right_tape : string option list;
  pos : int;
}

type direction = Left | Right | Neutral

let move_tape tape direction =
  let new_pos =
    match direction with
    | Neutral -> tape.pos
    | Left -> tape.pos - 1
    | Right -> tape.pos + 1
  in

  let right_tape =
    if new_pos >= 0 && new_pos >= List.length tape.right_tape then
      tape.right_tape @ [ None ]
    else tape.right_tape
  in

  let left_tape =
    if new_pos < 0 && -new_pos > List.length tape.right_tape then
      tape.left_tape @ [ None ]
    else tape.left_tape
  in
  { right_tape; left_tape; pos = new_pos }

let rec list_update index f = function
  | [] -> []
  | h :: t -> if index = 0 then f h :: t else h :: list_update (index - 1) f t

let write_tape tape word =
  if tape.pos >= 0 then
    {
      tape with
      right_tape = list_update tape.pos (fun _ -> word) tape.right_tape;
    }
  else
    (* -1 because the left tape begins and positon -1*)
    {
      tape with
      left_tape = list_update (-tape.pos - 1) (fun _ -> word) tape.left_tape;
    }

let current_word tape =
  if tape.pos >= 0 then List.nth tape.right_tape tape.pos
  else
    (* -1 because the left tape begins and positon -1*)
    List.nth tape.right_tape (-tape.pos - 1)

let init_tape words =
  {
    right_tape = List.map (fun w -> Some w) words @ [ None ];
    left_tape = [ None ];
    pos = 0;
  }

type 'a turing_machine = {
  initial : 'a;
  rejet : 'a;
  accept : 'a;
  transition : 'a -> string option -> 'a * string option * direction;
  current_state : 'a;
  tape : tape;
}

type 'a turing_result = Accept | Reject | Continue of 'a turing_machine

let step turing_machine =
  let state, letter =
    (turing_machine.current_state, current_word turing_machine.tape)
  in
  let new_state, word_to_write, direction =
    turing_machine.transition state letter
  in
  if new_state = turing_machine.rejet then Reject
  else if new_state = turing_machine.accept then Accept
  else
    Continue
      {
        turing_machine with
        current_state = new_state;
        tape =
          move_tape (write_tape turing_machine.tape word_to_write) direction;
      }

type end_result = Yes | No

let run turing_machine words =
  let rec aux tm =
    match step tm with Accept -> Yes | Reject -> No | Continue tm -> aux tm
  in
  let tm = { turing_machine with tape = init_tape words } in
  aux tm

type states_1 = Q0 | Q1 | Q2 | Q3

let example_machine =
  let transition s w =
    match (s, w) with
    | Q0, Some "0" -> (Q1, Some "X", Right)
    | Q1, Some "0" -> (Q1, Some "0", Right)
    | Q2, Some "0" -> (Q2, Some "0", Left)
    (* *)
    | Q1, Some "1" -> (Q2, Some "Y", Left)
    (* *)
    | Q2, Some "X" -> (Q0, Some "X", Right)
    (* *)
    | Q0, Some "Y" -> (Q3, Some "Y", Right)
    | Q1, Some "Y" -> (Q1, Some "Y", Right)
    | Q2, Some "Y" -> (Q2, Some "Y", Left)
    | Q3, Some "Y" -> (Q3, Some "Y", Right)
    (* *)
    | _ -> failwith "oops"
  in
  {
    initial = Q0;
    rejet = Q3;
    accept = Q3;
    transition;
    current_state = Q1;
    tape = { left_tape = []; right_tape = []; pos = 0 };
  }
