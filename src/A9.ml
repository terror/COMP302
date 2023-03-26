(* Question 1 *)

let infer_op (op : primop) (ts : tp list) : tp =
  match op with
  | Plus | Minus | Times ->
      if List.length ts != 2 then raise ArityMismatch
      else if not (List.for_all (fun x -> x = Int) ts) then raise TypeMismatch
      else Int
  | Negate ->
      if not (match ts with [x] -> x != Int | _ -> raise ArityMismatch) then
        raise TypeMismatch
      else Int
  | Equals ->
      if List.length ts != 2 then raise ArityMismatch
      else if
        (not (List.for_all (fun x -> x = Int) ts))
        || not (List.for_all (fun x -> x = Bool) ts)
      then raise TypeMismatch
      else Bool
  | LessThan ->
      if List.length ts != 2 then raise ArityMismatch
      else if not (List.for_all (fun x -> x = Int) ts) then raise TypeMismatch
      else Bool

(* Question 2 *)

let infer_tests : ((ctx * exp) * tp) list =
  [ (([], Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1]))), Arrow ([Int], Int))
  ; ( ([], Fn ([("x", Bool); ("y", Int)], If (Var "x", Var "y", I 0)))
    , Arrow ([Bool; Int], Int) )
  ; (([], Apply (Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1])), [I 42])), Int)
  ; (([], Apply (Fn ([("x", Bool)], If (Var "x", I 1, I 0)), [B true])), Int) ]

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
      raise NotImplemented
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
