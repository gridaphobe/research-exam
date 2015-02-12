// http://www.pex4fun.com/default.aspx?language=FSharp&code=GASdU9tqwkAQfc9XnCfJtlFqH4sKfSkUWhHaH4g6MaEbVzYT3Eg%2fvntJvLdI8xBmz8yZM5fdUi1rSZhptdJpGakNrfHRVEyt%2fV4stKpUxoMZmcGLjaGt0l9RxM2GwJoI4wj4xhulmTemaklQWfDdoViz%2fbuD9UaSGEovETOePCg83cGaFlgpbMAeAsqUF7k9bQvOPRBE0J9YZk0t5OVimZhEC_fawKDXc5nirF6jCVjjsAYjGAEJT73_%2fUI1ltoIaMss1vsI09UiwFHUNVEVOzpv8KyXQycP0bUmhrgPaWRn6G56TiGnYpXziQb_J1KmBnGbToq9qcWx3DyVt2u1q7mUc1mkm6UztDPidF4d1NHHXh2jMYbiMNJ0aa_MsRXY2yRunm2owJ0SmMTD4qS0pi2tyMKCwbm98sHrJA1kgiYJ47j8SFYUuJNTbmCFDH9xj6MPI5%2fVu519ktf7dbO3L%2fG5qkjz4LWaKp7WUsYszhyfdg%2fdbNkW_Hg1wL1F7pbic4Sq_Qc%3d

module Program
open System
open Microsoft.Pex.Framework

type tree =
  | Leaf
  | Node of tree * int * tree
  
let ord (t : tree) =
  let rec go p t =
    match t with
    | Leaf -> true
    | Node (l,x,r) -> p x && go (fun y -> p y && y < x) l 
                          && go (fun y -> p y && x < y) r
  in go (fun x -> true) t

let rec size (t : tree) =
  match t with
  | Leaf -> 0
  | Node (l,x,r) -> 1 + size l + size r
  
let rec height (t : tree) = 
  match t with
  | Leaf -> 0
  | Node (l,x,r) -> 1 + max (height l) (height r)
  
let rec bal (t : tree) = 
  match t with
  | Leaf -> true
  | Node (l,x,r) -> bal l && bal r && (abs (height l - height r) <= 1)

let rec add (x : int) (t : tree) =
  match t with
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (l,y,r) -> if x < y then Node (add x l, y, r)
                    else if x > y then Node (l, y, add x r)
                    else Node (l, y, r)
  
let Puzzle (x : int) (t : tree) = 
  PexAssert.IsNotNull(t)
  PexAssert.IsTrue(height t > 2)
  PexAssert.IsTrue(ord t && bal t)
  add x t
