(* Question 1 *)

let infer_op (op : primop) (ts : tp list) : tp =
  match op with
  | Negate -> (
    match ts with
    | [x] ->
        if x != Int then raise TypeMismatch else Int
    | _ ->
        raise ArityMismatch )
  | Equals -> (
    match ts with
    | [x; y] ->
        if not ((x = Int && y = Int) || (x = Bool && y = Bool)) then
          raise TypeMismatch
        else Bool
    | _ ->
        raise ArityMismatch )
  | LessThan -> (
    match ts with
    | [x; y] ->
        if not (x = Int && y = Int) then raise TypeMismatch else Bool
    | _ ->
        raise ArityMismatch )
  | _ -> (
    match ts with
    | [x; y] ->
        if not (x = Int && y = Int) then raise TypeMismatch else Int
    | _ ->
        raise ArityMismatch )

(* Question 2 *)

let infer_tests : ((ctx * exp) * tp) list =
  [ (([], Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1]))), Arrow ([Int], Int))
  ; ( ([], Fn ([("x", Bool); ("y", Int)], If (Var "x", Var "y", I 0)))
    , Arrow ([Bool; Int], Int) )
  ; (([], Apply (Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1])), [I 42])), Int)
  ; (([], Apply (Fn ([("x", Bool)], If (Var "x", I 1, I 0)), [B true])), Int)
  ; ( ( []
      , Rec
          ( "f"
          , Arrow ([Int], Int)
          , Fn
              ( [("x", Int)]
              , If
                  ( Primop (Equals, [Var "x"; I 0])
                  , I 1
                  , Primop
                      ( Times
                      , [ Var "x"
                        ; Apply (Var "f", [Primop (Minus, [Var "x"; I 1])]) ] )
                  ) ) ) )
    , Arrow ([Int], Int) ) ]

let rec infer (ctx : ctx) (e : exp) : tp =
  match e with
  | I _ ->
      Int
  | B _ ->
      Bool
  | Var x ->
      (* Look up `x` in ctx, raise FreeVariable if its not present *)
      raise NotImplemented
  | Primop (op, es) ->
      infer_op op (List.map (infer ctx) es)
  | If (cond, e1, e2) ->
      raise NotImplemented
  | Let (x, e1, e2) ->
      raise NotImplemented
  | Fn (xs, e') ->
      raise NotImplemented
  | Apply (e', args) ->
      raise NotImplemented
  | Rec (f, t, e') ->
      raise NotImplemented
