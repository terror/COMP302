(* Question 1: Manhattan Distance *)

let distance_tests = [
  (((2, 2), (2, 2)), 0);
  (((20, 30), (10, 20)), 20);
  (((10, 20), (20, 30)), 20);
  (((1, 1), (3, 2)), 3);
  (((-3, 2), (-50, 1)), 48);
]

let distance (x1, y1) (x2, y2) =
  abs (x2 - x1) + abs (y2 - y1)

(* Question 2: Binomial *)

let binomial_tests = [
  ((3, 2), 3);
  ((20, 10), 11);
  ((1, 1), 1);
  ((0, 0), 1);
  ((9, 2), 36);
  ((1, 0), 1);
]

let binomial n k =
  let rec factorial n =
    if n <= 1 then 1 else n * factorial (n - 1)
  in factorial n / (factorial k * factorial (n - k))

(* Question 3: Lucas Numbers *)

let lucas_tests = [
  ((0), 2);
  ((1), 1);
  ((3), 4);
  ((10), 123);
]

let rec lucas_helper n a b =
  match n with
    0 -> a
  | 1 -> b
  | n -> lucas_helper (n - 1) b (a + b)

let lucas n =
  lucas_helper n 2 1
