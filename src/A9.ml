(* Question 1 *)

let infer_op (op : primop) (ts : tp list) : tp =
  match op with
  | Plus | Minus | Times ->
      if List.length ts != 2 then raise ArityMismatch
      else if not (List.for_all (fun x -> x = Int) ts) then raise TypeMismatch
      else Int
  | Negate ->
      if List.length ts != 1 then raise ArityMismatch
      else if
        let (x :: _) = ts in
        x != Int
      then raise TypeMismatch
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

let infer_tests : ((ctx * exp) * tp) list = []

let rec infer (ctx : ctx) (e : exp) : tp =
  match e with
  | I _ ->
      raise NotImplemented
  | B _ ->
      raise NotImplemented
  | Var x ->
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
