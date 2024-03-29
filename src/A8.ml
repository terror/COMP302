(* Types in MiniCAML *)
type tp =
  | Arrow of tp list * tp (* function type: S1 S2 ... Sn -> T *)
  | Int
  | Bool

(* Used for variables, aka "identifiers" *)
type name = string

(* The primitive operations available in MiniCAML *)
type primop = Equals | LessThan | Plus | Minus | Times | Negate

(* Expressions in MiniCAML *)
type exp =
  | I of int (* 0 | 1 | 2 | ... *)
  | B of bool (* true | false *)
  | If of exp * exp * exp (* if e then e1 else e2 *)
  | Primop of primop * exp list (* e1 <op> e2 or <op> e *)
  | Fn of (name * tp) list * exp (* fn (x_1: t_1, ..., x_n: t_n) => e *)
  | Rec of name * tp * exp (* rec (f: t) => e *)
  | Let of name * exp * exp (* let x = e1 in e2 end *)
  | Apply of exp * exp list (* e (e_1, e_2, ..., e_n) *)
  | Var of name (* x *)

(* Deletes every occurence of the elements of xs from l. e.g. delete [w; y] [y;
   x; y; z; w] = [x; z] *)
let delete (xs : 'a list) (l : 'a list) : 'a list =
  List.filter (fun x -> not (List.mem x xs)) l

(* Generating fresh (new) variable names *)
type gen_var =
  { fresh: name -> name (* generates a fresh name based on a given one. *)
  ; reset: unit -> unit (* resets the internal counter for making names. *) }

let gen_var : gen_var =
  let counter = ref 0 in
  let fresh x =
    incr counter ;
    x ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  {fresh; reset}

let freshVar = gen_var.fresh

let resetCtr = gen_var.reset

(* Question 1 *)

let free_variables_tests =
  [ (Fn ([("x", Int); ("y", Int)], Primop (Plus, [Var "x"; Var "y"])), [])
  ; (Apply (Var "f", [Var "x"; Var "y"]), ["f"; "x"; "y"])
  ; (Rec ("f", Int, Apply (Var "f", [I 1])), [])
  ; (Let ("y", Primop (Plus, [Var "x"; I 1]), Var "y"), ["x"]) ]

let union l1 l2 = delete l2 l1 @ l2

let rec free_variables : exp -> name list = function
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

and union_fvs es =
  List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es

(* Question 2 *)

let subst_tests : (((exp * name) * exp) * exp) list =
  [ ( ((I 1, "x"), Rec ("f", Int, Primop (Times, [Var "x"; Var "f"])))
    , Rec ("f", Int, Primop (Times, [I 1; Var "f"])) )
  ; ( ((I 1, "f"), Rec ("f", Int, Primop (Times, [Var "x"; Var "f"])))
    , Rec ("f", Int, Primop (Times, [Var "x"; Var "f"])) )
  ; ( ((Var "f", "x"), Rec ("f", Int, Primop (Times, [Var "x"; Var "f"])))
    , Rec ("g", Int, Primop (Times, [Var "f"; Var "g"])) )
  ; (((I 1, "y"), Fn ([("x", Int)], Var "y")), Fn ([("x", Int)], I 1))
  ; (((I 1, "x"), Fn ([("x", Int)], Var "x")), Fn ([("x", Int)], Var "x"))
  ; (((Var "x", "y"), Fn ([("x", Int)], Var "y")), Fn ([("z", Int)], Var "x"))
  ; (((I 1, "x"), Apply (Var "x", [I 1])), Apply (I 1, [I 1]))
  ; (((I 1, "x"), Apply (Var "x", [Var "x"; Var "x"])), Apply (I 1, [I 1; I 1]))
  ]

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
      let y', exp = rename y e in
      Rec (y', t, subst s exp)
  | Fn (xs, e) ->
      let xs', exp = rename_all (List.map fst xs) e in
      Fn (List.combine xs' (List.map snd xs), subst s exp)
  | Apply (e, es) ->
      Apply (subst s e, List.map (fun e -> subst s e) es)

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
      let name', exp' = rename name exp in
      (name' :: names, exp') )
    names ([], exp)
