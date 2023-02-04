(* Question 1: Tree Depth *)

let tree_depth_cps_tests : (int tree * int) list =
  [ (Empty, 0)
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), 2)
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , 3 ) ]

let rec tree_depth t =
  match t with
  | Empty ->
      0
  | Tree (l, _, r) ->
      1 + max (tree_depth l) (tree_depth r)

let tree_depth_cps (t : 'a tree) =
  let rec helper (t : 'a tree) (sc : int -> int) =
    match t with
    | Empty ->
        sc 0
    | Tree (l, _, r) ->
        helper l (fun a -> helper r (fun b -> sc (1 + max a b)))
  in
  helper t (fun x -> x)

(* Question 2(a): Tree Traversal *)

(* TODO: Write a good set of tests for testing your tree traversal function. *)

let traverse_tests : (int tree * int list) list =
  [ (Empty, [])
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), [2; 3; 1])
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , [2; 4; 3; 1] ) ]

(* TODO: Implement a CPS style postorder traversal function. *)

let traverse (tree : 'a tree) : 'a list =
  let rec helper (tree : 'a tree) (sc : unit -> 'a list) =
    raise NotImplemented
  in
  raise NotImplemented

(* Question 2(b): Distances from the Root *)

(* TODO: Write a good set of tests for testing the get_distances function. *)

let get_distances_tests : (int tree * int list) list = []

(* TODO: Implement a CPS style get_distances function. *)

let get_distances (tree : int tree) : int list =
  let rec helper tree sum sc = raise NotImplemented in
  raise NotImplemented

(* Question 3: Finding Subtrees *)

(* TODO: Write a good set of tests for finding subtrees. *)

let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [ (* Your test cases go here *) ]

(* TODO: Implement a CPS style find_subtree_cont function.*)

let find_subtree_cps ls tree =
  let rec helper ls tree sc fc = raise NotImplemented in
  raise NotImplemented
