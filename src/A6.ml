(* Q1 : String to Characters to String *)

let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s)

let string_implode (l : char list) : string =
  List.fold_right ( ^ ) (List.map (String.make 1) l) ""

(* Q2 : Bank Account *)

let open_account (pass : password) : bank_account = raise NotImplemented
