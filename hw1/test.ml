open Hw1;;

let incG a = a + 1;;

let addG a b = a + b;;

let subG a b = 
    if (a < b) then failwith "first argument must be greater then second"
    else a - b;;

let mulG a b = a * b;;

let rec powerG a b = 
    if (b = 0) then 1
    else a * (powerG a (b - 1));;
    
print_string "Test 1: Inc\n"
let a = 423;;
let res = incG a in
let res2 = int_of_peano(inc (peano_of_int a)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;
    
print_string "Test 2: Add\n"
let a = 344;;
let b = 3452;;
let res = addG a b in
let res2 = int_of_peano(add (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 3: Sub\n"
let a = 23462;;
let b = 14234;;
let res = subG a b in
let res2 = int_of_peano(sub (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 4: Mul\n"
let a = 55;;
let b = 355;;
let res = mulG a b in
let res2 = int_of_peano(mul (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 5: Power\n"
let a = 2;;
let b = 15;;
let res = powerG a b in
let res2 = int_of_peano(power (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 6: Zero Inc\n"
let a = 423;;
let res = incG a in
let res2 = int_of_peano(inc (peano_of_int a)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 7: Zero power\n"
let a = 23;;
let b = 0;;
let res = powerG a b in
let res2 = int_of_peano(power (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 8: Zero power or zero power??\n"
let a = 0;;
let b = 0;;
let res = powerG a b in
let res2 = int_of_peano(power (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 9: Zero Mul\n"
let a = 55;;
let b = 0;;
let res = mulG a b in
let res2 = int_of_peano(mul (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 10: Zero Sub\n"
let a = 23462;;
let b = 23462;;
let res = subG a b in
let res2 = int_of_peano(sub (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 11: Zero Add\n"
let a = 344;;
let b = 0;;
let res = addG a b in
let res2 = int_of_peano(add (peano_of_int a) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;

print_string "Test 12: ...?\n"
let a = 23;;
let b = 564;;
let c = 111;;
let d = 567;;
let res = subG (addG (incG (mulG a c)) d) b in
let res2 = int_of_peano(sub (add (inc (mul (peano_of_int a) (peano_of_int c))) (peano_of_int d)) (peano_of_int b)) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ".\n")
else print_string "OK.\n";;
print_string "_________________________\n";;
print_string "Test 13: Are you real G?\n"
let a = 23;;
let b = 25;;
let c = 2;;
let d = 480;;
let res = addG (subG (mulG ((incG (powerG b c))) (powerG c (powerG c c))) d) (mulG a a) in
let res2 = int_of_peano(add (sub (mul (inc (power (peano_of_int b) (peano_of_int c))) (power (peano_of_int c) (power (peano_of_int c) (peano_of_int c)))) (peano_of_int d)) (mul (peano_of_int a) (peano_of_int a))) in
if (res <> res2) then print_string ("Failed. Expected: " ^ string_of_int(res) ^ ". Found: " ^ string_of_int(res2) ^ ". You're not a real G.\n")
else print_string "You're real G. Congrats!\n";;
print_string "_________________________\n";;

print_string "Test 14: Int to Peano\n"
let a = 5;;
let res = S (S (S (S (S Z)))) in
let res2 = peano_of_int a in
if (res <> res2) then print_string ("Failed.")
else print_string "Everything is OK. Congrats!\n";;
print_string "_________________________\n";;

print_string "Test 15: Zero Int to Peano\n"
let a = 0;;
let res = Z in
let res2 = peano_of_int a in
if (res <> res2) then print_string ("Failed.")
else print_string "Everything is OK. Congrats!\n";;
print_string "_________________________\n";;