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

let traverse_tests : (int tree * int list) list =
  [ (Empty, [])
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), [2; 3; 1])
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , [2; 4; 3; 1] ) ]

let traverse (tree : 'a tree) : 'a list =
  let rec helper (tree : 'a tree) (sc : unit -> 'a list) =
    match tree with
    | Empty ->
        sc ()
    | Tree (l, x, r) ->
        helper l (fun () -> helper r (fun () -> x :: sc ()))
  in
  helper tree (fun () -> [])

(* Question 2(b): Distances from the Root *)

let get_distances_tests : (int tree * int list) list =
  [ (Empty, [])
  ; (Tree (Tree (Empty, 2, Empty), 1, Empty), [3; 1])
  ; (Tree (Empty, 1, Tree (Empty, 2, Empty)), [3; 1])
  ; (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)), [3; 4; 1])
  ; ( Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
    , [3; 8; 4; 1] ) ]

let get_distances (tree : int tree) : int list =
  let rec helper tree sum sc =
    match tree with
    | Empty ->
        sc ()
    | Tree (l, x, r) ->
        helper l (x + sum) (fun () ->
            helper r (x + sum) (fun () -> (x + sum) :: sc ()) )
  in
  helper tree 0 (fun () -> [])

(* Question 3: Finding Subtrees *)

let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [ (([], Empty), Some Empty)
  ; (([1], Empty), None)
  ; ( ([1], Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)))
    , Some (Tree (Empty, 2, Empty)) )
  ; ( ( [1; 3]
      , Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Tree (Empty, 4, Empty)))
      )
    , Some Empty )
  ; ( ( [1; 3]
      , Tree (Tree (Empty, 2, Empty), 1, Tree (Tree (Empty, 4, Empty), 3, Empty))
      )
    , Some (Tree (Empty, 4, Empty)) ) ]

let find_subtree_cps ls tree =
  let rec helper ls tree sc fc = raise NotImplemented in
  raise NotImplemented
