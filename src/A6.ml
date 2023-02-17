(* Q1 : String to Characters to String *)

let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s)

let string_implode (l : char list) : string =
  List.fold_right ( ^ ) (List.map (String.make 1) l) ""

(* Q2 : Bank Account *)

let check count =
  match !count with
  | 3 ->
      raise account_locked
  | _ ->
      count := !count + 1 ;
      raise wrong_pass

let update_pass password count old_pass new_pass =
  match old_pass = !password with
  | true ->
      password := new_pass
  | false ->
      check count

let deposit password count balance pass amount =
  match pass = !password with
  | true ->
      if amount < 0 then raise negative_amount else balance := !balance + amount
  | false ->
      check count

let retrieve password count balance pass amount =
  match pass = !password with
  | true ->
      if !balance - amount < 0 then raise not_enough_balance
      else if amount < 0 then raise negative_amount
      else balance := !balance - amount
  | false ->
      check count

let show_balance password count balance pass =
  match pass = !password with true -> !balance | false -> check count

let open_account (pass : password) : bank_account =
  let password = ref pass in
  let balance = ref 0 in
  let count = ref 0 in
  { update_pass= update_pass password count
  ; deposit= deposit password count balance
  ; retrieve= retrieve password count balance
  ; show_balance= show_balance password count balance }
