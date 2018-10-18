
type algebraic_term = Var of string
                    | Fun of string * (algebraic_term list)
 
let counter = ref 0;;
 
let next_var() = let result = "tmp" ^ string_of_int !counter in
                     counter := !counter + 1;
                     result;;
 
let rec term_to_string (at : algebraic_term) =
    let rec term_to a = match a with
                      | Var x     -> x
                      | Fun(f, l) -> f ^ "(" ^ list_to l ^ ")"
 
    and list_to l = match l with
                  | []      -> ""
                  | (x::[]) -> term_to x
                  | (x::xs) -> (term_to x) ^ " " ^ (list_to xs)
    in
    term_to at;;
 
 
let rec contains str at msk = match at with
                            | (Var a)      -> if a = str
                                              then msk lor 1
                                              else msk
                            | (Fun (f, l)) -> (contains_list str l msk) lor (if str = f
                                                                             then 2
                                                                             else 0)
and contains_list str l msk = match l with
                            | []      -> msk
                            | (x::xs) -> (contains str x msk) lor (contains_list str xs msk);;
 
let rec separate s l r = match s with
                       | []           -> l, r
                       | (ls, rs)::xs -> separate xs (ls::l) (rs::r);;
 
let system_to_equation s =
    let l, r = separate s [] [] in
    let newname = next_var() in
    (Fun(newname, l), Fun(newname, r));;
 
module StringMap = Map.Make (String);;
 
let apply_substitution x y =
    let rec apply_subst a map = match a with
                              | Var v     -> if StringMap.mem v map
                                             then StringMap.find v map
                                             else (Var v)
                              | Fun(v, u) -> Fun(v, apply_list u map)
 
    and apply_list a map =
        let rec appl a c = match a with
                         | []    -> List.rev c
                         | z::zs -> appl zs ((apply_subst z map)::c) in
        appl a [] in
 
    let rec mkmap l m = match l with
                      | []           -> m
                      | (ls, rs)::xs -> mkmap xs (StringMap.add ls rs m) in
 
    apply_subst y (mkmap x StringMap.empty);;
 
let rec apply_substitution2A x y = (apply_substitution x (fst y), apply_substitution x (snd y));;
 
let rec is_equal x y = match (x, y) with
                     | (Var a, Var b)           -> a = b
                     | (Fun(f, ax), Fun(g, by)) -> (f = g) && (is_equal_list ax by)
                     | _                        -> false
 
and is_equal_list x y = match (x, y) with
                      | ([], [])         -> true
                      | (a::ax), (b::by) -> (is_equal a b) && (is_equal_list ax by)
                      | _                -> false;;
 
let check_solution x y =
    let toeq = system_to_equation y in
    is_equal (apply_substitution x (fst toeq)) (apply_substitution x (snd toeq));;
 
let solve_system x =
    let rec unwrap sys subst = match sys with
                             | []           -> subst
                             | (a, b)::tail -> unwrap tail ((a, (apply_substitution subst b))::subst) in
    let rec is_rule_ok var eq = match eq with
                              | Var t     -> (t = var)
                              | Fun(a, b) -> List.fold_left ((fun var boolean elem -> (boolean
                                                                                   || is_rule_ok var elem)) var) false b
                             in
    let rec solve x subst = match x with
                          | []           -> (true, subst)
                          | (a, b)::tail -> (match (a, b) with
                                               | (Var xs, ys)             -> if (ys = Var xs)
                                                                             then solve tail subst
                                                                             else
                                                                                 if (is_rule_ok xs ys )
                                                                                 then (false, subst)
                                                                                 else
                                                                                    if ((List.fold_left ((
                                                                                                        fun var boolean subst -> (((fst subst = fst var)
                                                                                                                 && (snd subst <> snd var))
                                                                                                                 || boolean)) (xs, ys)) false subst))
                                                                                    then (false, subst)
                                                                                    else
                                                                                        let sub = (xs, ys)::subst in
                                                                                        let tail = List.map (apply_substitution2A sub) tail in
                                                                                        solve tail sub
                                               | (xs, Var ys)             -> solve ((b, a)::tail) subst
                                               | (Fun(xs, u), Fun(ys, v)) -> if ((List.length u <> List.length v) || (xs = ys))
                                                                             then solve (List.append (List.combine u v) tail) subst
                                                                             else (false, subst)) in
                          if (fst (solve x []))
                          then let answer = (snd (solve x [])) in
                               Some (unwrap answer []);
                          else None;;