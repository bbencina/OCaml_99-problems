(*1*)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl

(*2*)
let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some (x, y)
  | _ :: tl -> last_two tl

(*3*)
(* Starting index here is for some reason 1. *)
let rec at k = function
  | [] -> None
  | hd :: tl -> if k = 1
                then Some hd
                else at (k-1) tl

(*4*)
let length l =
  let rec counting i = function
    | [] -> i
    | _ :: tl -> counting (i+1) tl
  in counting 0 l

(*5*)
let rev l =
  let rec reving acc = function
    | [] -> acc
    | hd :: tl -> reving (hd :: acc) tl
  in reving [] l

(*6*)
let is_palindrome l =
  l = rev l

(*7*)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten l =
  let rec flattening acc = function
    | [] -> acc
    | One x :: tl -> flattening (x :: acc) tl
    | Many xs :: tl -> flattening (flattening acc xs) tl
  in rev (flattening [] l)

(*8*)
let rec compress = function
  | hd :: (x :: _ as tl) -> if hd = x
                            then compress tl
                            else hd :: compress tl
  | l -> l

(*9*)
let pack l =
  let rec packing sub acc = function
    | [] -> []
    | [x] -> (x :: sub) :: acc
    | hd :: (x :: _ as tl) -> if hd = x
                              then packing (hd :: sub) acc tl
                              else packing [] ((hd :: sub) :: acc) tl
  in rev (packing [] [] l)

(*10*)
let encode l =
  let rec counting count acc = function
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | hd :: (x :: _ as tl) -> if hd = x
                              then counting (count+1) acc tl
                              else counting 0 ((count+1, hd) :: acc) tl
    in rev (counting 0 [] l)
