module percolation

open System

type Site = Closed | Open | Full
type PercolationModel = array<int*Site> * int

let createModel n : PercolationModel =
    let last = n*n+1
    let ids = [|0..last|] |> Array.map (fun i -> (i,Closed))
    Array.set ids 0 (0,Open)
    Array.set ids last (last,Open)
    ids,n

let validate i j n =
    if i <= 0 || j <= 0 || i > n || j > n then raise (new ArgumentException())

let openSite i j model =
    let n = snd model
    validate i j n
    let ids = fst model
    let index = (i-1)*n + j
    Array.set ids index (index, Open)
    ids,n

