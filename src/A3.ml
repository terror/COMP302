(* Question 1a *)

let to_int_tests : (int church * int) list =
  [ ((fun _ z -> z), 0)
  ; ((fun s z -> s z), 1)
  ; ((fun s z -> s (s (s (s (s z))))), 5) ]

let to_int (n : int church) : int = n (fun x -> x + 1) 0

(* Question 1b *)

let add_tests : (('b church * 'b church) * 'b church) list =
  [ (((fun _ z -> z), fun _ z -> z), fun _ z -> z)
  ; (((fun s z -> s z), fun _ z -> z), fun s z -> s z)
  ; (((fun _ z -> z), fun s z -> s z), fun s z -> s z) ]

let add (n1 : 'b church) (n2 : 'b church) : 'b church = fun s z -> n1 s (n2 s z)

(* Question 1c *)

let mult_tests : (('b church * 'b church) * 'b church) list =
  [ (((fun _ z -> z), fun _ z -> z), fun _ z -> z)
  ; (((fun s z -> s z), fun _ z -> z), fun _ z -> z)
  ; (((fun _ z -> z), fun s z -> s z), fun _ z -> z)
  ; ( ((fun s z -> s (s z)), fun s z -> s (s (s z)))
    , fun s z -> s (s (s (s (s (s z))))) ) ]

let mult (n1 : 'b church) (n2 : 'b church) : 'b church = raise NotImplemented

(* Question 2a *)

let is_even_tests : ('b church * bool) list = []

let is_even (n : 'b church) : bool = raise NotImplemented

(* Question 2b: Generate a list whose length is given by a church numeral with
   one element over and over *)
(* TODO: Test cases;
 * You only need to test lists of int here. *)
let gen_list_tests : (('b church * int) * int list) list = []

let gen_list (n : 'b church) (x : 'a) : 'a list = raise NotImplemented
