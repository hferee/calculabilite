open Core

type states_1 = Q0 | Q1 | Q2 | Q3

let _example_machine_1 =
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
  { initial = Q0; rejet = Q3; accept = Q3; transition }

type states_2 = Q0 | Q1 | Q2 | Q3 | Q4

let example_machine_2 =
  let transition s w =
    match (s, w) with
    | Q0, Some "0" -> (Q1, None, Right)
    | Q0, Some "x" -> (Q4, Some "x", Right)
    | Q0, None -> (Q3, None, Right)
    (* *)
    | Q1, Some "0" -> (Q1, Some "0", Right)
    | Q1, Some "x" -> (Q1, Some "x", Right)
    | Q1, Some "1" -> (Q2, Some "x", Left)
    (* *)
    | Q2, Some "0" -> (Q2, Some "0", Left)
    | Q2, Some "x" -> (Q2, Some "x", Left)
    | Q2, None -> (Q0, None, Right)
    (* *)
    | Q4, None -> (Q3, None, Left)
    | Q4, Some "x" -> (Q4, Some "x", Right)
    | _ -> failwith "oops"
  in
  { initial = Q0; rejet = Q3; accept = Q3; transition }
