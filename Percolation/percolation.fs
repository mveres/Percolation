module percolation

open System

type Site = Closed | Open | Full
type PercolationModel = array<int*Site> * int


let createModel n : PercolationModel =
    let last = n*n+1
    let ids = [|0..last|] |> Array.map (fun i -> (i,Closed))
    Array.set ids 0 (0, Full)
    Array.set ids last (last, Open)
    ids,n


let validate i j model =
    let n = snd model
    if i <= 0 || j <= 0 || i > n || j > n then raise (new ArgumentException())


let getIndex i j model =
        let n = snd model
        (i-1)*n + j


let openSite i j model =
    validate i j model

    let index = getIndex i j model
    let ids = fst model

    Array.set ids index (index, Open)
    ids, (snd model)


let siteMatches expectedSite i j (model: PercolationModel) =
    validate i j model

    let index = getIndex i j model
    let ids = fst model

    ids.[index] |> snd |> (=) expectedSite


let isOpen i j model =
    siteMatches Open i j model


let isFull i j model =
    siteMatches Full i j model
