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

(*11*)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_with_t l =
  let tuple_it_up i ent =
    if i = 1
    then One ent
    else (Many (i, ent))
  in
  let rec counting count acc = function
    | [] -> []
    | [x] -> (tuple_it_up (count+1) x) :: acc
    | hd :: (x :: _ as tl) -> if hd = x
                              then counting (count+1) acc tl
                              else counting 0 ((tuple_it_up (count+1) hd) :: acc) tl
  in rev (counting 0 [] l)

(*12*)
let decode_with_t l =
  let rec append_t lst = function
    | One x | Many (1, x) -> x :: lst
    | Many (i, x) -> append_t (x :: lst) (Many (i-1, x))
  in
  let rec decoding acc = function
    | [] -> acc
    | hd :: tl -> decoding (append_t acc hd) tl
  in decoding [] (rev l)

(*13*)
let encode_with_t_direct l =
  let to_rle i x =
    if i = 0
    then One x
    else Many (i+1, x)
  in
  let rec encoding count acc = function
    | [] -> []
    | [x] -> (to_rle count x) :: acc
    | hd :: (x :: _ as tl) -> if hd = x
                              then encoding (count+1) acc tl
                              else encoding 0 ((to_rle count hd) :: acc) tl
  in rev (encoding 0 [] l)

(*14*)
let rec duplicate = function
  | [] -> []
  | hd :: tl -> hd :: hd :: (duplicate tl)

(*15*)
let replicate l n =
  let rec nplicate n acc x =
    if n = 0
    then acc
    else nplicate (n-1) (x :: acc) x
  in
  let rec replicating acc = function
    | [] -> acc
    | hd :: tl -> replicating (nplicate n acc hd) tl
  in replicating [] (rev l)

(*16*)
let drop l n =
  let rec dropping acc l i =
    match l, i with
    | [], _ -> acc
    | hd :: tl, 1 -> dropping acc tl n
    | hd :: tl, j -> dropping (hd :: acc) tl (j-1)
  in rev (dropping [] l n)

(*17*)
let split l n =
  let rec listing acc l i =
    match l, i with
    | [], _ -> (rev acc, [])
    | l, 0 -> (rev acc, l)
    | hd :: tl, i -> listing (hd :: acc) tl (i-1)
  in listing [] l n

(*18*)
(*A bit uglier yet tail recursive solution than in the official solutions.*)
let slice l i k =
  let rec from_beginning l n=
    match l, n with
    | l, 0 -> l
    | [], _ -> []
    | hd :: tl, n -> from_beginning tl (n-1)
  in from_beginning (rev (from_beginning (rev l) ((length l)-k-1))) i

(*19*)
(*Again, a bit uglier yet tail recursive.*)
let rotate l n =
  let len = length l
  in
  let n =
    if len = 0
    then 0
    else ((n mod len) + len) mod len
  in
  if n = 0
  then l
  else (
    let l1, l2 = split l n
    in
    let rec dodajanje l1 l2 =
      match l2 with
      | [] -> l1
      | hd :: tl -> dodajanje (hd :: l1) tl
    in dodajanje l1 (rev l2)
    )

(*20*)
let rec remove_at n = function
  | [] -> []
  | hd :: tl -> if n = 0
                then tl
                else hd :: (remove_at (n-1) tl)

(*21*)
let rec insert_at x n = function
  | [] -> [x]
  | hd :: tl as l -> if n = 0
                      then x :: l
                      else hd :: (insert_at x (n-1) tl)

(*22*)
(*Tail recursive version.*)
let range m1 m2 =
  let rec ranging acc a b =
    if a = b
    then rev (a :: acc)
    else ranging (a :: acc) (a+1) b
  in
  if m1 <= m2
  then ranging [] m1 m2
  else rev (ranging [] m2 m1)
