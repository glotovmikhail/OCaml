open Hw1;;

module SetOfString = Set.Make(String);;
module MyMap       = Map.Make(String);;

let rec free_vars_set expr notFree = 
	match expr with
			  Var v     -> if SetOfString.mem v notFree
			 		       then SetOfString.empty
			   		       else SetOfString.singleton v
			| Abs(v, x) -> free_vars_set x (SetOfString.add v notFree)
			| App(x, y) -> SetOfString.union (free_vars_set x notFree) (free_vars_set y notFree);;

let free_vars expr = let newSet = free_vars_set expr SetOfString.empty
					 in  SetOfString.elements newSet;; 

let rec is_free free_vars not_free expr var = 
	match expr with
			  Var v     -> if v = var
			  			   then ((SetOfString.inter free_vars not_free) = SetOfString.empty)
			  			   else true
			| Abs(v, x) -> if v = var
						   then true
						   else is_free free_vars (SetOfString.add v not_free) x var
			| App(x, y) -> (is_free free_vars not_free x var) && (is_free free_vars not_free y var);;

let free_to_subst what where var = let set_of_free_vars = free_vars_set what SetOfString.empty
								   in  is_free set_of_free_vars SetOfString.empty where var;;

let rec is_normal_form expr = 
	match expr with
			  Var x 			 -> true
			| App(x, y) 		 -> (is_normal_form x) && (is_normal_form y)
			| App(Abs(x, ys), y) -> false
			| Abs(x, y)    		 -> is_normal_form y

let is_alpha_equivalent expr1 expr2 = 
	let rec var_subst expr new_var old_var =
			match expr with
				     Var v 	   -> if v = old_var
				     		      then Var(new_var)
				     		      else Var(v)
				   | Abs(v, x) -> if v = old_var
				   				  then Abs(new_var, var_subst x new_var old_var)
				   				  else Abs(v, var_subst x new_var old_var)
				   | App(x, y) -> App(var_subst x new_var old_var, var_subst y new_var old_var) in
	let counter = ref 0 in
	let next_var() = let result = "temp" ^ string_of_int !counter 
					 in
					     counter := !counter + 1;
					   	 result in
	let rec equivalent expr1 expr2 =
		match (expr1, expr2) with
					  (Var v, Var u)         -> if v = u
					  							then true
					  							else false
					| (Abs(v, x), Abs(u, y)) -> 
							let new_var = next_var()
							in
							    equivalent (var_subst x new_var v) (var_subst y new_var u)
					| (App(x, y), App(a, b)) -> 
							if (equivalent x a && equivalent y b)
							then true
							else false
					| _						 -> false in

	equivalent expr1 expr2;;

print_string (String.concat " " (free_vars (lambda_of_string "\\x.xy")) ^ "\n")