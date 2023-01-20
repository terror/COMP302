(* Question 1 *)

let q1a_nat_of_int_tests : (int * nat) list =
  [(0, Z); (1, S Z); (5, S (S (S (S (S Z)))))]

let q1a_nat_of_int (n : int) : nat =
  let rec aux n acc = match n with 0 -> acc | n -> aux (n - 1) (S acc) in
  aux n Z

let q1b_int_of_nat_tests : (nat * int) list =
  [(Z, 0); (S Z, 1); (S (S (S (S (S Z)))), 5)]

let q1b_int_of_nat (n : nat) : int =
  let rec aux n acc = match n with Z -> acc | S n -> aux n (acc + 1) in
  aux n 0

let q1c_add_tests : ((nat * nat) * nat) list =
  [((Z, Z), Z); ((Z, S Z), S Z); ((S Z, Z), S Z); ((S Z, S Z), S (S Z))]

let rec q1c_add (n : nat) (m : nat) : nat =
  match (n, m) with
  | Z, Z ->
      m
  | Z, S _ ->
      m
  | S _, Z ->
      n
  | S a, S _ ->
      q1c_add a (S m)

(* Question 2 *)

let q2a_neg (e : exp) : exp = Times (e, Const (-1.0))

let q2b_minus (e1 : exp) (e2 : exp) : exp = Plus (e1, q2a_neg e2)

let rec q2c_pow (e1 : exp) (p : nat) : exp =
  match p with Z -> Const 1.0 | S n -> Times (e1, q2c_pow e1 n)

(* Question 3 *)

let eval_tests : ((float * exp) * float) list =
  [ ((1.0, Const 2.0), 2.0)
  ; ((1.0, Var), 1.0)
  ; ((2.0, Plus (Var, Div (Const 2.0, Times (Var, Const 1.0)))), 3.0)
  ; ((-2.0, Plus (Var, Div (Const 2.0, Times (Var, Const 1.0)))), -3.0) ]

let rec eval (a : float) (e : exp) : float =
  match e with
  | Const x ->
      x
  | Var ->
      a
  | Plus (lhs, rhs) ->
      eval a lhs +. eval a rhs
  | Times (lhs, rhs) ->
      eval a lhs *. eval a rhs
  | Div (lhs, rhs) ->
      eval a lhs /. eval a rhs

(* Question 4 *)

let diff_tests : (exp * exp) list =
  [ (Const 1.0, Const 0.0)
  ; (Var, Const 1.0)
  ; ( Div (Times (Var, Const (-1.0)), Const 0.0)
    , Div
        ( Plus
            ( Times
                ( Plus (Times (Const 1., Const (-1.)), Times (Var, Const 0.))
                , Const 0. )
            , Times (Times (Times (Var, Const (-1.)), Const 0.), Const (-1.)) )
        , Times (Const 0., Const 0.) ) )
  ; (Plus (Var, Var), Plus (Const 1.0, Const 1.0))
  ; (Times (Var, Var), Plus (Times (Const 1.0, Var), Times (Var, Const 1.0)))
  ; ( Div (Var, Var)
    , Div
        ( Plus (Times (Const 1.0, Var), q2a_neg (Times (Var, Const 1.0)))
        , Times (Var, Var) ) ) ]

let rec diff (e : exp) : exp =
  match e with
  | Const _ ->
      Const 0.0
  | Var ->
      Const 1.0
  | Plus (lhs, rhs) ->
      Plus (diff lhs, diff rhs)
  | Times (lhs, rhs) ->
      Plus (Times (diff lhs, rhs), Times (lhs, diff rhs))
  | Div (lhs, rhs) ->
      Div
        ( Plus (Times (diff lhs, rhs), q2a_neg (Times (lhs, diff rhs)))
        , Times (rhs, rhs) )
