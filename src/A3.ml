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

let mult (n1 : 'b church) (n2 : 'b church) : 'b church = fun s z -> n1 (n2 s) z

(* Question 2a *)

let is_even_tests : ('b church * bool) list =
  [ ((fun _ z -> z), true)
  ; ((fun s z -> s z), false)
  ; ((fun s z -> s (s z)), true) ]

let is_even (n : 'b church) : bool = n (fun x -> x + 1) 0 mod 2 = 0

(* Question 2b *)

let gen_list_tests : (('b church * int) * int list) list =
  [ (((fun _ z -> z), 0), [])
  ; (((fun s z -> s z), 0), [0])
  ; (((fun s z -> s (s z)), 0), [0; 0]) ]

let gen_list (n : 'b church) (x : 'a) : 'a list = raise NotImplemented
