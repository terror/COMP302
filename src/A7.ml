(* Question 1.1 *)

let rec take (n : int) (s : 'a stream) : 'a list =
  match n with 0 -> [] | n -> s.head :: take (n - 1) (force s.tail)

(* Question 1.2 *)

let lucas1 = str_map fst (iterate (fun (a, b) -> (b, a + b)) (2, 1))

(* Question 1.3 *)

let rec unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  raise NotImplemented

(* Question 1.3 *)

let lucas : int stream = int_stream_not_implemented

(* Question 2.1 *)

let rec scale (s1 : int stream) (n : int) : int stream = raise NotImplemented

let rec merge (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  raise NotImplemented

(* Question 2.2 *)

let rec s = int_stream_not_implemented
