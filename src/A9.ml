(* Types in MiniCAML *)
type tp =
  | Arrow of tp list * tp (* function type: S1 S2 ... Sn -> T *)
  | Int
  | Bool

(* Used for variables, aka "identifiers" *)
type name = string

(* The primitive operations available in MiniCAML *)
type primop = Equals | LessThan | Plus | Minus | Times | Negate

(* Expressions in MiniCAML *)
type exp =
  | I of int (* 0 | 1 | 2 | ... *)
  | B of bool (* true | false *)
  | If of exp * exp * exp (* if e then e1 else e2 *)
  | Primop of primop * exp list (* e1 <op> e2 or <op> e *)
  | Fn of (name * tp) list * exp (* fn (x_1: t_1, ..., x_n: t_n) => e *)
  | Rec of name * tp * exp (* rec (f: t) => e *)
  | Let of name * exp * exp (* let x = e1 in e2 end *)
  | Apply of exp * exp list (* e (e_1, e_2, ..., e_n) *)
  | Var of name (* x *)

(* Context is simply a list of variable names and their types *)
type ctx = (name * tp) list

(* Exceptions to raise in type inference *)
exception ArityMismatch

exception TypeMismatch

exception FreeVariable

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
  ; (([], Apply (Fn ([("x", Int)], Primop (Plus, [Var "x"; I 1])), [I 1])), Int)
  ; (([], Apply (Fn ([("x", Bool)], If (Var "x", I 1, I 0)), [B true])), Int)
  ; ( ( []
      , Apply
          ( Fn ([("x", Int); ("y", Bool)], If (Var "y", Var "x", I 0))
          , [I 1; B true] ) )
    , Int )
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
    , Arrow ([Int], Int) )
  ; (([], Apply (Fn ([], Primop (Plus, [I 2; I 3])), [])), Int)
  ; (([], Fn ([], Primop (Plus, [I 2; I 3]))), Arrow ([], Int)) ]

let rec infer (ctx : ctx) (e : exp) : tp =
  match e with
  | I _ ->
      Int
  | B _ ->
      Bool
  | Var x -> (
    match List.assoc_opt x ctx with Some y -> y | None -> raise FreeVariable )
  | Primop (op, es) ->
      infer_op op (List.map (infer ctx) es)
  | If (cond, e1, e2) -> (
    match infer ctx cond with
    | Bool ->
        let a = infer ctx e1 in
        let b = infer ctx e2 in
        if a = b then a else raise TypeMismatch
    | _ ->
        raise TypeMismatch )
  | Let (x, e1, e2) ->
      infer ((x, infer ctx e1) :: ctx) e2
  | Fn (xs, e') ->
      Arrow (List.map snd xs, infer (xs @ ctx) e')
  | Apply (e', args) -> (
    match infer ctx e' with
    | Arrow (arg_types, ret) ->
        if List.length arg_types = List.length args then
          if arg_types = List.map (infer ctx) args then ret
          else raise TypeMismatch
        else raise ArityMismatch
    | _ ->
        raise TypeMismatch )
  | Rec (f, t, e') ->
      if t = infer ((f, t) :: ctx) e' then t else raise TypeMismatch
