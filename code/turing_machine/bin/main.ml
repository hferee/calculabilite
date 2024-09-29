open Turing_machine.Core
open Turing_machine.Examples_tm


let word_of_string s = String.to_seq s |> Seq.map (String.make 1) |> List.of_seq

let () =
  (match word_of_string "01" |> run example_machine_2 with
  | Yes -> print_char 'Y'
  | No -> print_char 'N');
  print_newline ()
