(* Question 1 *)

let free_variables_tests =
  [ (Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"])), [])
  ; (Apply (Var "f", [Var "x"; Var "y"]), ["f"; "x"; "y"])
  ; (Rec ("f", Int, Apply (Var "f", [I 1])), [])
  ; (Let ("y", Primop (Plus, [Var "x"; I 1]), Var "y"), ["x"]) ]

let rec free_variables : exp -> name list =
  let union l1 l2 = delete l2 l1 @ l2 in
  let union_fvs es =
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y ->
      [y]
  | I _ | B _ ->
      []
  | If (e, e1, e2) ->
      union_fvs [e; e1; e2]
  | Primop (_, args) ->
      union_fvs args
  | Fn (xs, e) ->
      delete (List.map fst xs) (union_fvs [e])
  | Rec (x, _, e) ->
      delete [x] (union_fvs [e])
  | Let (x, e1, e2) ->
      delete [x] (union_fvs [e1; e2])
  | Apply (e, es) ->
      union_fvs ([e] @ es)

(* Question 2 *)

(* Note: we've added a type annotation here so that the compiler can help you
   write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list =
  [ (* An example test case. If you have trouble writing test cases of the
       proper form, you can try copying this one and modifying it. Note that you
       are *only* required to write tests for Rec, Fn, and Apply! *)
    ( ( (I 1, "x")
      , (* [1/x] *)
        (* let y = 2 in y + x *)
        Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"])) )
    , (* let y = 2 in y + 1 *)
      Let ("y", I 2, Primop (Plus, [Var "y"; I 1])) ) ]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e' else Var y
  | I n ->
      I n
  | B b ->
      B b
  | Primop (po, args) ->
      Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then Let (y, e1', e2)
      else
        let y, e2 =
          if List.mem y (free_variables e') then rename y e2 else (y, e2)
        in
        Let (y, e1', subst s e2)
  | Rec (y, t, e) ->
      raise NotImplemented
  | Fn (xs, e) ->
      raise NotImplemented
  | Apply (e, es) ->
      raise NotImplemented

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
      let name', exp' = rename name exp in
      (name' :: names, exp') )
    names ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)

let subst_list subs exp = List.fold_left (fun exp sub -> subst sub exp) exp subs
