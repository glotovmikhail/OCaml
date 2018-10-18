open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;

type simp_type = S_Elem  of string | S_Arrow  of simp_type * simp_type
type hm_lambda = HM_Var  of string | HM_Abs   of string * hm_lambda  | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type   = HM_Elem of string | HM_Arrow of hm_type * hm_type   | HM_ForAll of string * hm_type

module MapOfString = Map.Make(String);;
module SetOfString = Set.Make(String);;

exception MyExp of string;;  

let generator_numbers2 n = Stream.from (fun i -> if i < n then Some i else None);;

let type_counter = ref 0;;
let next_type() = let result = "type" ^ string_of_int !type_counter 
				 in
				     type_counter := !type_counter + 1;
				   	 result;;

let counter = ref 0;;
let next_var() = let result = "temp" ^ string_of_int !counter 
				 in
				     counter := !counter + 1;
				   	 result;;

let rec term_of_simp_type t = 
	match t with
	| S_Elem(a) -> Hw2_unify.Var(a)
	| S_Arrow(a, b) -> Hw2_unify.Fun("->", [(term_of_simp_type a);(term_of_simp_type b)]);;

let rec simp_type_of_term t =
	match t with
	| Hw2_unify.Var(v) -> S_Elem(v)
	| Hw2_unify.Fun(name, [l;r]) -> S_Arrow(simp_type_of_term l, simp_type_of_term r)
	| _ -> failwith "Never";;

let rec typing_of_vars l map = 
	match l with
	| [] -> map
	| (h::t) -> typing_of_vars t (MapOfString.add h (S_Elem(next_type())) map);;

let rec get_system lmd map_of_types = match lmd with
	| Hw1.Var(v) -> ([], MapOfString.find v map_of_types)
	| Hw1.App(l1, l2) -> let (stm1, t1) = get_system l1 map_of_types in
						 let (stm2, t2) = get_system l2 map_of_types in
						 let nt = S_Elem(next_type()) in
							(stm1 @ stm2 @ [(t1, S_Arrow(t2, nt))], nt)
	| Hw1.Abs(v, l) -> let new_map = MapOfString.add v (S_Elem (next_type())) map_of_types in
					   let stm1, t1 = get_system l new_map in
						   (stm1, S_Arrow(MapOfString.find v new_map, t1));;

let free_vars hm_lambda = 
	let rec impl hm_lambda set = 
		match hm_lambda with
		| HM_Var(a) -> if SetOfString.mem a set then SetOfString.empty else SetOfString.singleton a
		| HM_App(a, b) -> SetOfString.union (impl a set) (impl b set)
		| HM_Abs(a, b) -> impl b (SetOfString.add a set)
		| HM_Let(a, b, c) -> SetOfString.union (impl b set) (impl c (SetOfString.add a set))
	in impl hm_lambda SetOfString.empty;;

let free_types hm_type = 
	let rec impl hm_type set =
		match hm_type with
		| HM_Elem(a) -> if SetOfString.mem a set then SetOfString.empty else SetOfString.singleton a
		| HM_Arrow(a, b) -> SetOfString.union (impl a set) (impl b set)
		| HM_ForAll(a, b) -> impl b (SetOfString.add a set)
	in impl hm_type SetOfString.empty;;

	  
let infer_simp_type lmd =
	let stm, t = get_system lmd (typing_of_vars (Hw1_reduction.free_vars lmd) MapOfString.empty) in
	match Hw2_unify.solve_system (List.map (fun (a, b) -> (term_of_simp_type a, term_of_simp_type b)) stm) with
	  | Some solution -> Some (List.map (fun (a, b) -> (a, simp_type_of_term b)) solution, simp_type_of_term (Hw2_unify.apply_substitution solution (term_of_simp_type t)))
	  | None -> None;;

let rec term_from_hm hm_type =
	match hm_type with
	| HM_Elem(a) -> Hw2_unify.Var(a)
	| HM_Arrow(a, b) -> Hw2_unify.Fun("->", [(term_from_hm a);(term_from_hm b)])
	| _ -> failwith "cant convert";;
		 
let rec hm_from_term term = 
	match term with
	| Hw2_unify.Var(a) -> HM_Elem(a)
	| Hw2_unify.Fun(name, [l;r]) -> HM_Arrow(hm_from_term l, hm_from_term r)
	| _ -> failwith "cant convert";;

let algorithm_w hm_lmd = 
	let addU hm_type types = 
		let availble_types = MapOfString.fold (fun a b set -> SetOfString.union (free_types b) set) types SetOfString.empty in
			SetOfString.fold (fun a b -> HM_ForAll(a, b)) (SetOfString.fold (fun a b -> if SetOfString.mem a availble_types then b else SetOfString.add a b) (free_types hm_type) SetOfString.empty) hm_type in
	
	let rec substitute subst hm_type set =
		match hm_type with
		| HM_Elem(a) -> if SetOfString.mem a set then hm_type else
							if MapOfString.mem a subst then MapOfString.find a subst else hm_type
		| HM_Arrow(a, b) -> HM_Arrow(substitute subst a set, substitute subst b set)
		| HM_ForAll(a, b) -> HM_ForAll(a, substitute subst b (SetOfString.add a set)) in

	let types_subst subst types = 
		MapOfString.fold (fun a b map -> MapOfString.add a (substitute subst b SetOfString.empty) map) types MapOfString.empty in

	let rec removeU hm_type = 
		match hm_type with
		| HM_ForAll(a, b) -> substitute (MapOfString.add a (HM_Elem(next_var())) MapOfString.empty) (removeU b) SetOfString.empty
		| _ -> hm_type in
	
	let subst_to_subst subst1 subst2 = 
		MapOfString.fold (fun a b map -> 	if MapOfString.mem a map 
											then map 
											else MapOfString.add a b map) subst1
				(MapOfString.fold (fun a b map -> MapOfString.add a (substitute subst1 b SetOfString.empty) map)
								  subst2 MapOfString.empty) in

	let rec impl hm_lambda types = 
		match hm_lambda with
		| HM_Var(a) -> if MapOfString.mem a types then (removeU (MapOfString.find a types), MapOfString.empty) else raise (MyExp "Free variable encountered")
		| HM_App(a, b) ->
		   (let (hmt1, t1) = impl a types in
			let (hmt2, t2) = impl b (types_subst t1 types) in
			let new_type = HM_Elem (next_var()) in
			match solve_system ([((term_from_hm (substitute t2 hmt1 SetOfString.empty)), (term_from_hm (HM_Arrow(hmt2, new_type))))]) with
		   | None -> raise (MyExp "Couldn't solve the system")
		   | Some ans -> let ans_types = subst_to_subst 
								(List.fold_left (fun map (str, term) -> MapOfString.add str (hm_from_term term) map) MapOfString.empty ans) (subst_to_subst t2 t1) in
						(substitute ans_types new_type SetOfString.empty, ans_types))
		| HM_Abs(a, b) -> let new_type = HM_Elem (next_var()) in
						let (hmt1, t1) = impl b (MapOfString.add a new_type (MapOfString.remove a types)) in
						(HM_Arrow(substitute t1 new_type SetOfString.empty, hmt1), t1)
		| HM_Let(a, b, c) -> let (hmt1, t1) = impl b types in
						   let new_types = types_subst t1 types in
						   let (hmt2, t2) = impl c (MapOfString.add a (addU hmt1 new_types) (MapOfString.remove a new_types)) in
						   (hmt2, subst_to_subst t2 t1) in
	let types = SetOfString.fold (fun a map -> MapOfString.add a (HM_Elem (next_var())) map) (free_vars hm_lmd) (MapOfString.empty) in
	try
		let (tp, map) = impl hm_lmd types in Some (MapOfString.bindings map, tp)
	with (MyExp e) -> None;;

let rec algebraic_term_to_string (at : algebraic_term) = 
	let rec impl a =
		match a with 
		| Var x -> x
		| Fun(f, l) -> f ^ "(" ^ impl_for_list l ^ ")" 
	
	and impl_for_list lt = 
		match lt with 
		| [] -> ""
		| (h::[]) -> impl h
		| (h::t) -> (impl h) ^ " " ^ (impl_for_list t)
	in
	impl at;;