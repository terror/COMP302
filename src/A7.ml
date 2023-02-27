(* Question 1.1 *)

let rec take (n : int) (s : 'a stream) : 'a list =
  match n with 0 -> [] | n -> s.head :: take (n - 1) (force s.tail)

(* Question 1.2 *)

let rec lucas1 = {head= 2; tail= Susp (fun () -> lucas2)}

and lucas2 = {head= 1; tail= Susp (fun () -> add_streams lucas1 lucas2)}

(* Question 1.3 *)

let unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  str_map (fun x -> fst (f x)) (iterate (fun x -> snd (f x)) seed)

(* Question 1.4 *)

let lucas : int stream = unfold (fun (a, b) -> (a, (b, a + b))) (2, 1)

(* Question 2.1 *)

let rec scale (s1 : int stream) (n : int) : int stream = raise NotImplemented

let rec merge (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  raise NotImplemented

(* Question 2.2 *)

let rec s = int_stream_not_implemented
