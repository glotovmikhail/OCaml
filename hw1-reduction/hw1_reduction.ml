open Hw1;;

module SetOfString = Set.Make(String);;
module MyMap = Map.Make(String);;

let counter = ref 0;;
let next_var() = let result = "temp" ^ string_of_int !counter 
				 in
				     counter := !counter + 1;
				   	 result;;

let rec find_free expr st = 
    match (expr) with 
    | Var(v) -> SetOfString.add v st;
    | App(l1, l2) -> SetOfString.union (find_free l1 st) (find_free l2 st);
    | Abs(v, l) -> SetOfString.remove v (find_free l st);;

let rec subst_expr what where x vars = 
	match (where) with 
    | Var(v) -> if (v = x) 
    			then what 
    			else Var(v);
    | App(l1, l2) -> App(subst_expr what l1 x vars, subst_expr what l2 x vars);
    | Abs(v, l) -> 	if (v = x) 
    				then Abs(v, l) 
    				else (	if (SetOfString.mem v vars) 
    						then failwith (v) 
    						else (Abs(v, subst_expr what l x vars)));;

type lambda_ref = | Var_ref of string 
                         | Abs_ref of (string * lambda_ref ref)
                         | App_ref of (lambda_ref ref * lambda_ref ref);;

let rec lambda_ref_from_lambda expr =
    match expr with
    | Var(v) -> ref (Var_ref(v))
    | App(l1, l2) -> ref (App_ref(lambda_ref_from_lambda l1, lambda_ref_from_lambda l2))
    | Abs(v, l) -> ref (Abs_ref(v, lambda_ref_from_lambda l));;

let rec lambda_from_lambda_ref imp_expr = 
    match !imp_expr with
    | Var_ref(v) -> Var(v)
    | App_ref(l1, l2) -> App(lambda_from_lambda_ref l1, lambda_from_lambda_ref l2)
    | Abs_ref(v, l) -> Abs(v, lambda_from_lambda_ref l);;
 
let rec subst_imp_expr what where x = 
    match !where with
    | Var_ref(v) -> 	if v = x 
    				then where := !what
    | App_ref(l1, l2) -> subst_imp_expr what l1 x; subst_imp_expr what l2 x
    | Abs_ref(v, l) -> 	if v <> x 
    					then subst_imp_expr what l x;;

let rec mpAdd expr map = 
    match expr with
	| Var(v) -> if MyMap.mem v map 
				then Var(MyMap.find v map) 
				else expr
	| App(l1, l2) -> App(mpAdd l1 map, mpAdd l2 map)
	| Abs(v, l) -> let newVar = next_var() in
					 Abs(newVar, mpAdd l (MyMap.add v newVar map));;	


let free_to_subst what where x = 
	try
	    let sl = subst_expr what where x (find_free what (SetOfString.empty)) in  
	    match sl with 
	    | _ -> true
	with 
		| _ -> false

let free_vars expr = 
    let vars = find_free expr SetOfString.empty in 
	SetOfString.elements vars;;

let rec is_alpha_equivalent x y =
	let rec subst_var newVar expr old = 
    	match (expr) with 
    	| Var(v) -> if v = old 
    				then Var(newVar) 
    				else Var(v);
   	 	| App(l1, l2) -> App(subst_var newVar l1 old, subst_var newVar l2 old);
    	| Abs(v, l) -> 	if v = old 
    					then Abs(newVar, subst_var newVar l old) 
    					else Abs(v, subst_var newVar l old) in
    match (x, y) with 
    | (Var(v), Var(u)) -> if v = u 
    						then true 
    						else false;
    | (App(v1, u1), App(v2, u2)) -> 
            if (is_alpha_equivalent v1 v2 && is_alpha_equivalent u1 u2) 
            then true else false;
    | (Abs(v1, l1), Abs(v2, l2)) -> 
            let newVar = next_var() in
            is_alpha_equivalent (subst_var newVar l1 v1) (subst_var newVar l2 v2); 
    | _ -> false;;

let rec is_normal_form expr =
    match expr with 
    | Var(x) -> true
    | Abs(v, l) -> is_normal_form l
    | App(l1, l2) -> 
        match l1 with 
        | Abs(_, _) -> false
        | _ -> is_normal_form l1 && is_normal_form l2;;		

let normal_beta_reduction expr =
	let rec impl expr = 
	  match expr with
	  | Var(x) -> (false, expr)
	  | Abs(x, l) -> let (ch, nb) = impl l in (ch, Abs(x, nb))
      | App(x, y) -> 
        match x with 
        | Abs(v, u) -> (true, subst_expr y u v (find_free y (SetOfString.empty)))
        | _ -> let (ch, na) = impl x in
					if ch then (ch, App(na, y))
					else let (ch, nb) = impl y in
						(ch, App(x, nb))
	in
	let (_, ans) = impl (mpAdd expr MyMap.empty) in
	ans;;

let rec reduce_to_normal_form expr = 
	let imp_expr = lambda_ref_from_lambda (mpAdd expr MyMap.empty) in
	let rec impl imp_expr = match !imp_expr with
	  | Var_ref(v) -> None
	  | App_ref(l1, l2) ->
		(match !l1 with					
		| Abs_ref(a, b) -> let newVar = next_var() in
						    imp_expr := !(lambda_ref_from_lambda(mpAdd (lambda_from_lambda_ref b) (MyMap.singleton a newVar)));
							subst_imp_expr l2 imp_expr newVar;
							Some imp_expr
		| _ -> match impl l1 with
			    | Some bexpr -> Some imp_expr
				| None -> match impl l2 with
								| Some bexpr -> Some imp_expr
								| None -> None)				
	  | Abs_ref(a, b) -> match impl b with
						        | Some bexpr -> Some imp_expr
						        | None -> None					
	in 
	let rec ret_impl imp_expr = match impl imp_expr with
	  | Some bexpr -> ret_impl bexpr
	  |	None -> imp_expr
	in
	lambda_from_lambda_ref (ret_impl imp_expr);;