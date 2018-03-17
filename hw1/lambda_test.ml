open Hw1;;
let str = "\\x.\\y.(x (x (\\z.z y)))";;
let l = lambda_of_string str;;
print_string(string_of_lambda l ^ "\n");;