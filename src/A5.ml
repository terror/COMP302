exception Not_implemented

type formula =
  | Variable of string
  | Conjunction of formula * formula
  | Disjunction of formula * formula
  | Negation of formula

module Variable_set = Set.Make (String)
module Variable_map = Map.Make (String)

let variable_set_to_list = Variable_set.elements

let variable_map_to_list = Variable_map.bindings

type truth_assignment = bool Variable_map.t

exception Unassigned_variable of string

exception Unsatisfiable_formula

let formula =
  Negation
    (Conjunction
       ( Variable "foo"
       , Disjunction
           ( Variable "bar"
           , Conjunction (Variable "baz", Negation (Variable "foo")) ) ) )

(* Question 1 *)

let collect_variables_tests : (formula * Variable_set.t) list =
  [(formula, Variable_set.of_list ["foo"; "bar"; "baz"])]

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

let rec make_map (values : ('a * 'b) list) : 'b Variable_map.t =
  match values with
  | [] ->
      Variable_map.empty
  | (a, b) :: xs ->
      Variable_map.add a b (make_map xs)

let eval_success_tests : ((truth_assignment * formula) * bool) list =
  [ ((make_map [("foo", true); ("bar", false); ("baz", true)], formula), true)
  ; ((make_map [("foo", true); ("bar", true); ("baz", true)], formula), false)
  ]

let eval_failure_tests : ((truth_assignment * formula) * exn) list =
  [ ( (make_map [("foo", true); ("bar", false)], formula)
    , Unassigned_variable "baz" ) ]

let rec eval (state : truth_assignment) (formula : formula) : bool =
  match formula with
  | Variable v -> (
    match Variable_map.find_opt v state with
    | Some result ->
        result
    | None ->
        raise (Unassigned_variable v) )
  | Conjunction (a, b) ->
      let lhs = eval state a in
      let rhs = eval state b in
      lhs && rhs
  | Disjunction (a, b) ->
      let lhs = eval state a in
      let rhs = eval state b in
      lhs || rhs
  | Negation a ->
      not (eval state a)

(* Question 3 *)

let find_satisfying_assignment_tests : (formula * truth_assignment option) list
    =
  [ (formula, Some (make_map [("foo", true); ("bar", false); ("baz", true)]))
  ; (Conjunction (Variable "foo", Negation (Variable "foo")), None)
  ; ( Negation
        (Disjunction
           (Variable "foo", Disjunction (Variable "bar", Variable "baz")) )
    , Some (make_map [("foo", false); ("bar", false); ("baz", false)]) ) ]

let find_satisfying_assignment (formula : formula) : truth_assignment =
  let rec aux l (acc : truth_assignment) =
    match l with
    | [] -> (
      match eval acc formula with
      | true ->
          acc
      | false ->
          raise Unsatisfiable_formula )
    | x :: xs -> (
      try aux xs (Variable_map.add x true acc)
      with Unsatisfiable_formula -> aux xs (Variable_map.add x false acc) )
  in
  aux (Variable_set.elements (collect_variables formula)) Variable_map.empty
