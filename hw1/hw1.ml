(* Peano *)
type peano = Z
	| S of peano;;

let rec peano_of_int x = match x with
	0 -> Z
	| _ -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
	Z -> 0
	| S x -> 1 + int_of_peano x;;

let inc a = S (a);;

let rec add a b = match b with
	Z -> a
	| S b' -> S (add a b');;

let rec sub a b = match b with
	Z -> a
	| S b' -> match a with
		Z -> Z
		|S a' -> sub a' b';;

let rec mul a b = match b with
	Z -> Z
	| S b' -> add (mul a b') a;;

let rec power a b = match b with
	Z -> S (Z)
	| S b' -> mul a (power a b');;

(* Lambda *)

type lambda = Var of string 
            | Abs of string * lambda 
            | App of lambda * lambda;;

let rec string_of_lambda l = match l with
    Var x -> x
    | Abs (s, l) -> "(\\" ^ s ^ "." ^ string_of_lambda l ^ ")"
    | App (l1, l2) -> "(" ^ string_of_lambda l1 ^ " " ^ string_of_lambda l2 ^ ")";;

let rec lambda_of_string s = 
    let s = s ^ ";" in
    let pos = ref 0 in
    let get() = s.[!pos] in
    let next() = 
        if !pos < String.length s - 1 then
            pos := !pos + 1 in
            
    let eat x = 
        if get() <> x then
            failwith "unexpected symbol"
        else 
            next() in
            
    let is_finished() = 
        if get() = ';' then
            true
        else 
            false in
    
    
    let parse_ident() = 
        let v = String.make 1 (get()) in
        next();
        v in
           
    let rec parse_lambda() = match get() with
        '\\' -> let l = parse_abs () in
            check l
        | '(' -> 
            eat '(';
            let l = parse_lambda() in
            eat ')';
            check l
        | _ -> let l = Var (parse_ident()) in
            check l
            
    and parse_abs () =
        eat '\\';
        let v = parse_ident() in
        eat '.';
        let l = parse_lambda() in
        Abs (v, l)
        
    and check l = 
        if (is_finished() || get() = ')') then
            l
        else
            App (l, parse_lambda()) in
            
    parse_lambda();;

(* List Funcs *)

let rev = 
    let rec rev_app acc l = match l with
        [] -> acc
        | h::t -> rev_app (h::acc) t in
    fun l -> rev_app [] l;;

let rec merge = function
    | list, []
    | [], list -> list
    | h1::t1, h2::t2 -> 
        if h1 > h2 then
            h2::merge (h1::t1, t2)
        else
            h1::merge (t1, h2::t2);;
let rec split = function
    | []
    | [_] as t1 -> t1, []
    | h::t -> 
        let t1, t2 = split t in
        h::t2, t1;;
        
let rec merge_sort = function
    | []
    | [_] as list -> list
    | list -> 
        let l1, l2 = split list in 
        merge (merge_sort l1, merge_sort l2);;

let rec print_list = function
    | [] -> ()
    | element::l -> print_int element ; print_string " " ; print_list l;;
    