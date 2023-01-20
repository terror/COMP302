(* Question 1 *)

let q1a_nat_of_int_tests : (int * nat) list = [
  (0, Z);
  (1, S Z);
  (5, S (S (S (S (S Z)))))
]

let rec q1a_nat_of_int (n : int) : nat =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | n -> aux (n - 1) (S acc)
  in aux n Z

let q1b_int_of_nat_tests : (nat * int) list = [
  (Z, 0);
  (S Z, 1);
  (S (S (S (S (S Z)))), 5)
]

let q1b_int_of_nat (n : nat) : int =
  let rec aux n acc =
    match n with
    | Z -> acc
    | S n -> aux n (acc + 1)
  in aux n 0

let q1c_add_tests : ((nat * nat) * nat) list = [
  ((Z, Z), Z);
  ((Z, S Z), S Z);
  ((S Z, Z), S Z);
  ((S Z, S Z), S (S Z))
]

let rec q1c_add (n : nat) (m : nat) : nat =
  match (n, m) with
  | (Z, Z) -> m
  | (Z, S _) -> m
  | (S _, Z) -> n
  | (S a, S _) -> q1c_add a (S m)

(* Question 2 *)

let q2a_neg (e : exp) : exp =
  Times (e, Const (-1.0))

let q2b_minus (e1 : exp) (e2 : exp) : exp =
  Plus (e1, q2a_neg e2)

let rec q2c_pow (e1 : exp) (p : nat) : exp =
  match p with
  | Z -> Const 1.0
  | S n -> Times (e1, q2c_pow e1 n)

(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = []

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = raise Not_implemented

(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = []

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = raise Not_implemented
