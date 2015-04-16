module percolation

open System

type Site = Closed | Open | Full
type Grid = Site [,]

let createGrid n =
    if n <= 0 then raise (ArgumentException())
    Array2D.create n n Closed


let validate i j grid =
    let n = Array2D.length1 grid
    let m = Array2D.length2 grid
    if m <> n then raise (ArgumentException("Percolation grid must be square"))
    if i < 0 || j < 0 || i >= n || j >= n then raise (ArgumentOutOfRangeException())


let openSite i j grid =
    validate i j grid
    Array2D.set grid i j Open
    grid


let isOpen i j grid =
    validate i j grid
    grid.[i,j] = Open


