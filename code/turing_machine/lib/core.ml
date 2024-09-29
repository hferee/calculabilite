type tape = {
  left_tape : string option list;
  right_tape : string option list;
  pos : int;
}

type direction = Left | Right | Neutral

let grow_if cond l = if cond then l @ [ None ] else l

let move_tape tape direction =
  let new_pos =
    match direction with
    | Neutral -> tape.pos
    | Left -> tape.pos - 1
    | Right -> tape.pos + 1
  in
  let right_tape =
    grow_if
      (new_pos >= 0 && new_pos >= List.length tape.right_tape)
      tape.right_tape
  in
  let left_tape =
    grow_if
      (new_pos < 0 && -new_pos > List.length tape.left_tape)
      tape.left_tape
  in
  { right_tape; left_tape; pos = new_pos }

let rec list_update index f = function
  | [] -> []
  | h :: t -> if index = 0 then f h :: t else h :: list_update (index - 1) f t

let write_at pos w = list_update pos (fun _ -> w)

let write_tape tape word =
  if tape.pos >= 0 then
    { tape with right_tape = write_at tape.pos word tape.right_tape }
  else
    (* -1 because the left tape begins at positon -1 *)
    { tape with left_tape = write_at (-tape.pos - 1) word tape.left_tape }

let current_word tape =
  if tape.pos >= 0 then List.nth tape.right_tape tape.pos
  else
    (* -1 because the left tape begins at positon -1 *)
    List.nth tape.left_tape (-tape.pos - 1)

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
}

type 'a configuration = { current_state : 'a; tape : tape }
type end_result = Yes | No
type 'a step_result = Stop of end_result | Continue of 'a configuration

let step turing_machine configuration =
  let state, letter =
    (configuration.current_state, current_word configuration.tape)
  in
  let new_state, word_to_write, direction =
    turing_machine.transition state letter
  in
  if new_state = turing_machine.rejet then Stop No
  else if new_state = turing_machine.accept then Stop Yes
  else
    Continue
      {
        current_state = new_state;
        tape = move_tape (write_tape configuration.tape word_to_write) direction;
      }

let run turing_machine words =
  let configuration =
    { current_state = turing_machine.initial; tape = init_tape words }
  in
  let rec aux configuration =
    match step turing_machine configuration with
    | Stop r -> r
    | Continue configuration -> aux configuration
  in
  aux configuration
