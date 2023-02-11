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

let make_map values =
  let rec aux values acc =
    match values with
    | [] ->
        acc
    | (a, b) :: xs ->
        aux xs (Variable_map.add a b acc)
  in
  aux values Variable_map.empty

let eval_success_tests : ((truth_assignment * formula) * bool) list =
  [ ( ( make_map [("foo", true); ("bar", false); ("baz", true)]
      , Negation
          (Conjunction
             ( Variable "foo"
             , Disjunction
                 ( Variable "bar"
                 , Conjunction (Variable "baz", Negation (Variable "foo")) ) )
          ) )
    , true )
  ; ( ( make_map [("foo", true); ("bar", true); ("baz", true)]
      , Negation
          (Conjunction
             ( Variable "foo"
             , Disjunction
                 ( Variable "bar"
                 , Conjunction (Variable "baz", Negation (Variable "foo")) ) )
          ) )
    , false ) ]

let eval_failure_tests : ((truth_assignment * formula) * exn) list =
  [ ( ( make_map [("foo", true); ("bar", false)]
      , Negation
          (Conjunction
             ( Variable "foo"
             , Disjunction
                 ( Variable "bar"
                 , Conjunction (Variable "baz", Negation (Variable "foo")) ) )
          ) )
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
  []

let find_satisfying_assignment (formula : formula) : truth_assignment =
  raise Not_implemented
