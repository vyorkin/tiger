open Base

type key = string

type tree =
  | Leaf
  | Tree of tree * key * tree

let empty = Leaf

let rec insert t k =
  let open Base.Poly in
  match t with
  | Leaf -> Tree (Leaf, k, Leaf)
  | Tree (l, k', r) ->
    if k < k'
    then Tree (insert l k, k', r)
    else if k > k'
    then Tree (l, k', insert r k)
    else Tree (l, k, r)

(* a *)

let rec member t k =
  match t with
  | Leaf -> false
  | Tree (l, k', r) ->
    let open Base.Poly in
    if k < k' then member l k
    else if k > k' then member r k
    else true
