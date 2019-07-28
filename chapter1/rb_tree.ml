open Base

(* Lets review Red-Black trees from the Chris Okasaki book *)

type color =
  | Red
  | Black

type 'a tree =
  | Leaf (* considered Black *)
  | Node of color * ('a tree) * 'a * ('a tree)

(* Invariants:
   1) No red node has a red child
   2) Every path from the root to an empty node
      contains the same number of black nodes
*)

let balance = function
  | (Black, Node (Red, Node (Red, a, x, b), y, c), z, d)
  | (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d)
  | (Black, a, x, Node (Red, Node (Red, b, y, c), z, d))
  | (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (color, a, x, b) -> Node (color, a, x, b)

let insert t x =
  let rec ins = function
    | Leaf -> Node (Red, Leaf, x, Leaf)
    | Node (color, a, y, b) as node ->
      let open Base.Poly in
      if x < y then balance (color, (ins a), y, b)
      else if x > y then balance (color, a, y, (ins b))
      else node
  in
  match ins t with
  | Node (_, a, y, b) -> Node (Black, a, y, b)
  | Leaf -> assert false

let rec member t k =
  match t with
  | Leaf -> false
  | Node (_, l, k', r) ->
    let open Base.Poly in
    if k < k' then member l k
    else if k > k' then member r k
    else true

let t1 = insert (insert (insert (insert (insert (insert Leaf "b") "a") "i") "p") "s") "t"
