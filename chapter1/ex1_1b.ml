open Base

type key = string

type 'a tree =
  | Leaf
  | Tree of ('a tree) * key * 'a * ('a tree)

let rec insert t k v =
  let open Base.Poly in
  match t with
  | Leaf -> Tree (Leaf, k, v, Leaf)
  | Tree (l, k', v', r) ->
    if k < k' then Tree (insert l k v, k', v', r)
    else if k > k' then Tree (l, k', v', insert r k v)
    else Tree (l, k, v, r)

let rec lookup t k =
  let open Base.Poly in
  match t with
  | Leaf -> None
  | Tree (l, k', v, r) ->
    if k = k' then Some v
    else if k < k' then lookup l k
    else lookup r k

let t1 = insert (insert (insert (insert Leaf "i" 4) "p" 3) "s" 2) "t" 1
