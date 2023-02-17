(* Q1 : String to Characters to String *)

let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s)

let string_implode (l : char list) : string =
  List.fold_right ( ^ ) (List.map (String.make 1) l) ""

(* Q2 : Bank Account *)

type state = {password: string ref; count: int ref; balance: int ref}

let verify (state : state) (candidate : password) (callback : unit -> 'a) : 'a =
  match !(state.password) = candidate with
  | true ->
      callback ()
  | false -> (
    match !(state.count) with
    | 3 ->
        raise account_locked
    | _ ->
        state.count := !(state.count) + 1 ;
        raise wrong_pass )

let update_pass (state : state) (old_password : password)
    (new_password : password) : unit =
  verify state old_password (fun () -> state.password := new_password)

let deposit (state : state) (password : password) (amount : int) : unit =
  verify state password (fun () ->
      if amount < 0 then raise negative_amount
      else state.balance := !(state.balance) + amount )

let retrieve (state : state) (password : password) (amount : int) : unit =
  verify state password (fun () ->
      if !(state.balance) - amount < 0 then raise not_enough_balance
      else if amount < 0 then raise negative_amount
      else state.balance := !(state.balance) - amount )

let show_balance (state : state) (password : password) : int =
  verify state password (fun () -> !(state.balance))

let open_account (password : password) : bank_account =
  let state = {password= ref password; count= ref 0; balance= ref 0} in
  { update_pass= update_pass state
  ; deposit= deposit state
  ; retrieve= retrieve state
  ; show_balance= show_balance state }
