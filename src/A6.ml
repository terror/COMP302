(* Q1 : String to Characters to String *)

let string_explode (s : string) : char list =
  tabulate (fun i -> String.get s i) (String.length s)

let string_implode (l : char list) : string =
  List.fold_right (fun a b -> a ^ b) (List.map (fun x -> String.make 1 x) l) ""

(* Q2 : Bank Account *)

let open_account (pass : password) : bank_account = raise NotImplemented
