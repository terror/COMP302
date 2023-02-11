(* Question 1 *)

let collect_variables_tests : (formula * Variable_set.t) list =
  [ ( Negation
        (Conjunction
           ( Variable "foo"
           , Disjunction
               ( Variable "bar"
               , Conjunction (Variable "baz", Negation (Variable "foo")) ) ) )
    , Variable_set.of_list ["foo"; "bar"; "baz"] ) ]

let collect_variables (formula : formula) : Variable_set.t =
  let rec aux (f : formula) (sc : Variable_set.t -> Variable_set.t) =
    match f with
    | Variable v ->
        sc (Variable_set.of_list [v])
    | Conjunction (a, b) | Disjunction (a, b) ->
        aux a (fun x -> aux b (fun y -> sc (Variable_set.union x y)))
    | Negation a ->
        aux a sc
  in
  aux formula (fun x -> x)

(* Question 2 *)

let eval_success_tests : ((truth_assignment * formula) * bool) list = []

let eval_failure_tests : ((truth_assignment * formula) * exn) list = []

let eval (state : truth_assignment) (formula : formula) : bool =
  raise Not_implemented

(* Question 3 *)

let find_satisfying_assignment_tests : (formula * truth_assignment option) list
    =
  []

let find_satisfying_assignment (formula : formula) : truth_assignment =
  raise Not_implemented
