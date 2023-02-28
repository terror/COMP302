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

let scale (s1 : int stream) (n : int) : int stream = str_map (fun x -> x * n) s1

let rec merge (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  { head= (if s1.head < s2.head then s1.head else s2.head)
  ; tail=
      Susp
        (fun () ->
          if s1.head = s2.head then merge (force s1.tail) (force s2.tail)
          else if s1.head < s2.head then merge (force s1.tail) s2
          else merge s1 (force s2.tail) ) }

(* Question 2.2 *)

let rec s : int stream =
  { head= 1
  ; tail= Susp (fun () -> merge (scale s 2) (merge (scale s 3) (scale s 5))) }
