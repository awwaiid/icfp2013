(* Let's interpret some proggies! *)

open Printf

exception Error of string

type node =
  Atom of string
  | Exp of node list

let rec parse_prog_fragment = function
  | "("::rest ->
    let (subtree, rest) = parse_prog_fragment rest in
    let (p, rest) = parse_prog_fragment rest in
    (Exp(subtree)::p), rest

  | ")"::rest -> [], rest
  
  (* End of the list! *)
  | [] -> [], []

  (* Other stuff *)
  | (_ as s)::rest ->
    let (p, rest) = parse_prog_fragment rest in
    ((Atom(s)::p), rest)


let rec string_of_prog = function
  | Exp(e) ->
    let e = List.map string_of_prog e in
    "(" ^ (
      List.fold_left (
        fun a b -> a ^ b ^ " "
      ) " " e
    ) ^ ")"
  | Atom(s) -> s

let split_list_on l r =
  List.flatten (
    List.map (
      fun str ->
        List.map ( fun r -> match r with
          | Str.Text(s) -> s
          | Str.Delim(s) -> s
        )
        (Str.full_split (Str.regexp r) str)
    ) l
  )

let tokenize_prog str =
  let tokens = Str.split (Str.regexp " ") str in
  let tokens = split_list_on tokens "(" in
  let tokens = split_list_on tokens ")" in
  tokens

let parse_prog str =
  let tokens = tokenize_prog str in
  let nodes, rest = parse_prog_fragment tokens in
  List.nth nodes 0

let rec eval_with_env prog env =
  match prog with
(*   | Exp( Atom("lambda") :: params ) -> eval_lambda env params *)

  | Exp( Atom("lambda") :: params ) -> eval_lambda env params
  | Exp( Atom("fold")   :: params ) -> eval_fold env params
  | Exp( Atom("if0")    :: params ) -> eval_if0 env params

  (* Binary ops *)
  | Exp( Atom("plus")   :: params ) -> eval_plus env params
  | Exp( Atom("and")    :: params ) -> eval_and env params
  | Exp( Atom("or")     :: params ) -> eval_or env params
  | Exp( Atom("xor")    :: params ) -> eval_xor env params

  (* Unary ops *)
  | Exp( Atom("not")   :: params ) -> eval_not env params
  | Exp( Atom("shl1")  :: params ) -> eval_shl1 env params
  | Exp( Atom("shr1")  :: params ) -> eval_shr1 env params
  | Exp( Atom("shr4")  :: params ) -> eval_shr4 env params
  | Exp( Atom("shr16") :: params ) -> eval_shr16 env params

  | Exp( _ ) -> raise (Error "GAAA")

  (* All other atoms are either numbers or vars *)
  | Atom(s) ->
    try
      Int64.of_string s
    with _ ->
      Hashtbl.find env s

and eval_lambda env params =
  let body = List.nth params 1 in
  eval_with_env body env

and eval_fold env params =
  let source   = ref (eval_with_env (List.nth params 0) env) in
  let accum    = ref (eval_with_env (List.nth params 1) env) in
  let lambda   = List.nth params 2 in
  let bindings, body = (
    match lambda with
    | Exp( Atom("lambda") :: (Exp(_) as b) :: r ) -> b, (List.nth r 0)
    | _ -> raise (Error "INVALID FOLD LAMBDA")
  ) in
  let (y, z) = (
    match bindings with
    | Exp( Atom(_ as y) :: Atom(_ as z) :: [] ) ->
      (y, z)
    | _ -> raise (Error "INVALID FOLD")
  ) in

  for b = 1 to 8 do
    let v = Int64.logand !source (Int64.of_string "0xFF") in
    source := Int64.shift_right_logical !source 8;
    Hashtbl.add env y v;
    Hashtbl.add env z !accum;
    accum := eval_with_env body env;
    Hashtbl.remove env y;
    Hashtbl.remove env z;
  done;
  !accum


and eval_if0 env params =
  let cond = eval_with_env (List.nth params 0) env in
  (if cond = Int64.zero
  then eval_with_env (List.nth params 1) env
  else eval_with_env (List.nth params 2) env)

and eval_plus env params =
  let left  = eval_with_env (List.nth params 0) env in
  let right = eval_with_env (List.nth params 1) env in
  Int64.add left right

and eval_and env params =
  let left  = eval_with_env (List.nth params 0) env in
  let right = eval_with_env (List.nth params 1) env in
  Int64.logand left right

and eval_or env params =
  let left  = eval_with_env (List.nth params 0) env in
  let right = eval_with_env (List.nth params 1) env in
  Int64.logor left right

and eval_xor env params =
  let left  = eval_with_env (List.nth params 0) env in
  let right = eval_with_env (List.nth params 1) env in
  Int64.logxor left right

and eval_not env params =
  let left  = eval_with_env (List.nth params 0) env in
  Int64.lognot left

and eval_shl1 env params =
  let left  = eval_with_env (List.nth params 0) env in
  Int64.shift_left left 1

and eval_shr1 env params =
  let left  = eval_with_env (List.nth params 0) env in
  Int64.shift_right_logical left 1

and eval_shr4 env params =
  let left  = eval_with_env (List.nth params 0) env in
  Int64.shift_right_logical left 4

and eval_shr16 env params =
  let left  = eval_with_env (List.nth params 0) env in
  Int64.shift_right_logical left 16

let eval prog input =
  let env = Hashtbl.create 10 in (* initially empty environment *)
  Hashtbl.add env "x" input;
  eval_with_env prog env

let to_hex_string n =
  let left_part = Int64.shift_right_logical n 32 in
  let left_part = sprintf "%08X" (Int64.to_int left_part) in
  let right_part = Int64.logand n (Int64.of_string "0xFFFFFFFF") in
  let right_part = sprintf "%08X" (Int64.to_int right_part) in
  "0x" ^ left_part ^ right_part

let main () =
(*   while true do *)
(*
    eprintf "INTERP: Reading program\n";
    flush_all ();
*)
    let prog = read_line () in
    let prog = parse_prog prog in
  (*   printf "Prog: %s\n" (string_of_prog prog); *)

(*
    eprintf "INTERP: Reading inputs\n";
    flush_all ();
*)
    let inputs = read_line () in
(*
    eprintf "INTERP: Got line.\n";
    flush_all ();
*)
    let inputs = Str.split (Str.regexp " ") inputs in
    let inputs = List.map Int64.of_string inputs in
(*
    eprintf "INTERP: Got input. Calculating...\n";
    flush_all ();
*)

    List.iter (fun i ->
      printf "%s\n" (to_hex_string (eval prog i));
(*
      eprintf "INTERP: Output result %s\n" (to_hex_string (eval prog i));
*)
      flush_all ()
    ) inputs;
    flush_all ()
(*    done  *)

let _ =
(*   main (); *)
  main ()
