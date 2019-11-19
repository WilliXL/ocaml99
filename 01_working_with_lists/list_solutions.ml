exception TODO of string

(* Solutions to all of the working with lists problems *)

(* 1. last : 'a list -> 'a option
Return the last element of a list. (easy) *)
let rec last = function
    | []    -> None
    | [x]   -> Some x
    | x::xs -> last xs


(* 2. last_two : 'a list -> ('a * 'a) option
Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
    | []    -> None
    | [x]   -> None
    | [x;y] -> Some (x,y)
    | x::xs -> last_two xs


(* 3. at : int -> 'a list -> 'a option
Find the k'th element of a list. (easy) *)
let rec at k = function
    | [] -> None
    | x::xs -> if k = 1 then Some x else at (k-1) xs


(* 4. length : 'a list -> int
Return the length of the list (easy) *)
let rec length = function
    | [] -> 0
    | x::xs -> 1 + length xs


(* 5. rev : 'a list -> 'a list
Reverse the input list (easy) *)
let rec rev =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux []


(* 6. is_palindrome : 'a list -> bool
Return whether or not the input list is a is_palindrome (easy) *)
let rec is_palindrome list =
    list = rev list


(* 7. flatten : 'a node list -> 'a list
Flatten the input node list *)
(* First, a definition of nested list structure (medium) *)
type 'a node = | One of 'a | Many of 'a node list

let rec flatten = function
    | [] -> []
    | x::xs ->
    (
        match x with
            One x -> x::(flatten xs)
        |   Many x -> (flatten x) @ (flatten xs)
    )


(* 8. compress : 'a list -> 'a list
Remove all consecutive duplicates in list (medium) *)
let rec compress = function
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> if x = y then compress (y::xs) else x::(compress (y::xs))


(* 9. pack : 'a list -> 'a list list
Pack consecutive dpulicates of list elements in sublists *)
let rec pack list =
    let rec aux dups curr = function
        | [] -> dups::curr
        | x::xs ->
            match dups with
                | [] -> aux [x] curr xs
                | y::ys -> if x = y then aux (x::dups) curr xs
                         else aux [x] (dups::curr) xs
    in
        List.rev (aux [] [] list)

(* BIG BRAIN *)
let rec pack_jerbear_BIG_BRAIN list =
    let rec aux curr = function
        | [] -> ([],[])
        | x::xs ->
            if x <> curr then ([],x::xs)
            else let (dups,xs') = aux x xs in (x::dups, xs')
    in
        (* rev aux [] list *)
        match list with
        |    [] -> []
        |   x::xs -> let (dups, xs') = aux x list in dups::(pack xs')







(* TEST CASES *)
let () = Printf.printf "Testing List Solutions\n"

let list = [1;2;3;4;5]
(* last : 'a list -> 'a option *)
let () = assert(last list = Some(5))
let () = assert(last [] = None)
let () = Printf.printf "Question 1 - PASSED\n"

(* last_two : 'a list -> ('a * 'a) option *)
let () = assert(last_two list = Some(4,5))
let () = assert(last_two [47] = None)
let () = assert(last_two [] = None)
let () = Printf.printf "Question 2 - PASSED\n"

(* at : int -> 'a list -> 'a option *)
let () = assert((at 20 list) = None)
let () = assert((at 3 list) = Some(3))
let () = assert((at 0 []) = None)
let () = Printf.printf "Question 3 - PASSED\n"

(* length : 'a list -> int *)
let () = assert(length list = 5)
let () = assert(length [5;4] = 2)
let () = Printf.printf "Question 4 - PASSED\n"

(* rev : a' list -> 'a list *)
let () = assert(rev list = List.rev list)
let () = Printf.printf "Question 5 - PASSED\n"

(* is_palindrome : a' list -> bool *)
let palindrome_list = [1;2;3;4;3;2;1]
let () = assert(is_palindrome palindrome_list = true)
let () = assert(is_palindrome list = false)
let () = Printf.printf "Question 6 - PASSED\n"

let nested_list = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
let () = assert(flatten nested_list = ["a"; "b"; "c"; "d"; "e"])
let () = Printf.printf "Question 7 - PASSED\n"

let to_compress_list = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let () = assert(compress to_compress_list = ["a"; "b"; "c"; "a"; "d"; "e"])
let () = Printf.printf "Question 8 - PASSED\n"

let () = assert(pack to_compress_list = [["a";"a";"a";"a"];["b"];["c";"c"];["a";"a"];["d"];["e";"e";"e";"e"]])
let () = Printf.printf "Question 9 - PASSED\n"