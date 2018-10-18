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
        let rec impl s =
            if (get ()) <> ' ' && (get ()) <> '.' && (get ()) <> ')' && not (is_finished ())  
                then 
                    let c = get() in
                    next();
                    impl (s ^ String.make 1 c)
                else s in
        impl "" in
           
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
                if (is_finished () || s.[!pos] = ')') 
                then l 
                else    (eat ' '; 
                        match (get ()) with 
                            '\\' -> 
                                    (let ans = parse_abs () in
                                    check (App (l, ans))) 
                            | '(' -> 
                                    (eat '(';
                                    let ans = parse_lambda () in
                                    eat ')'; 
                                    check (App(l, ans)))
                            | _ ->  
                                    (let ans = (Var (parse_ident())) in
                                    check (App(l, ans)))
                        ) in
            
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

(* let s1 = lambda_of_string("((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.(((l10 (\\l11.(\\l12.((l6 l11) l12)))) (\\l11.(\\l12.(l11 (l11 (l11 l12)))))) (\\l11.(\\l12.l12)))) (\\l10.(l0 (\\l11.(\\l12.(\\l13.(((l8 l12) l13) ((l11 (l4 l12)) ((l10 l12) l13)))))))))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\l13.(((l1 l12) l13) (((l1 l13) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))");;
let s2 = lambda_of_string("\\x1.a");;
print_string(string_of_lambda s1);; *)
