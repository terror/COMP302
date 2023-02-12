type 'a tree = Empty | Tree of 'a tree * 'a * 'a tree

(* Question 1: Tree Depth *)

let tree_depth_cps_tests : (int tree * int) list =
  [ (Empty, 0)
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), 2)
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , 3 ) ]

let rec tree_depth (t : int tree) =
  match t with
  | Empty ->
      0
  | Tree (l, _, r) ->
      1 + max (tree_depth l) (tree_depth r)

let tree_depth_cps (t : 'a tree) : int =
  let rec helper (t : 'a tree) (sc : int -> int) : int =
    match t with
    | Empty ->
        sc 0
    | Tree (l, _, r) ->
        helper l (fun a -> helper r (fun b -> sc (1 + max a b)))
  in
  helper t (fun x -> x)

(* Question 2(a): Tree Traversal *)

let traverse_tests : (int tree * int list) list =
  [ (Empty, [])
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), [2; 3; 1])
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , [2; 4; 3; 1] ) ]

let traverse (tree : 'a tree) : 'a list =
  let rec helper (tree : 'a tree) (sc : 'a list -> 'r) : 'r =
    match tree with
    | Empty ->
        sc []
    | Tree (l, x, r) ->
        helper l (fun a -> helper r (fun b -> sc (a @ b @ [x])))
  in
  helper tree (fun x -> x)

(* Question 2(b): Distances from the Root *)

let get_distances_tests : (int tree * int list) list =
  [ (Empty, [])
  ; (Tree (Tree (Empty, 2, Empty), 1, Empty), [3; 1])
  ; (Tree (Empty, 1, Tree (Empty, 2, Empty)), [3; 1])
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), [3; 4; 1])
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , [3; 8; 4; 1] ) ]

let get_distances (tree : int tree) : int list =
  let rec helper (tree : int tree) (sum : int) (sc : int list -> 'r) : int list
      =
    match tree with
    | Empty ->
        sc []
    | Tree (l, x, r) ->
        helper l (x + sum) (fun a ->
            helper r (x + sum) (fun b -> sc (a @ b @ [x + sum])) )
  in
  helper tree 0 (fun x -> x)

(* Question 3: Finding Subtrees *)

let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [ (([], Empty), Some Empty)
  ; (([1], Empty), None)
  ; ( ([1], Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)))
    , Some (Tree (Empty, 2, Empty)) )
  ; ( ([], Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)))
    , Some (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty))) )
  ; ( ( [1; 3]
      , Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
      )
    , Some Empty )
  ; ( ( [1; 3]
      , Tree (Tree (Empty, 2, Empty), 1, Tree (Tree (Empty, 4, Empty), 3, Empty))
      )
    , Some (Tree (Empty, 4, Empty)) ) ]

let find_subtree_cps (ls : 'a list) (tree : 'a tree) : 'r =
  let rec helper (ls : 'a list) (tree : 'a tree) (sc : 'a tree -> 'r)
      (fc : unit -> 'r) : 'r =
    match (tree, ls) with
    | (Empty | Tree (_, _, _)), [] ->
        sc tree
    | Tree (l, x, _), [x'] when x = x' ->
        sc l
    | Tree (l, x, r), x' :: xs when x = x' ->
        helper xs l sc (fun () -> helper xs r sc fc)
    | (Empty | Tree (_, _, _)), _ :: _ ->
        fc ()
  in
  helper ls tree (fun x -> Some x) (fun () -> None)
