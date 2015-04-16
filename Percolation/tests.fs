module tests

open NUnit.Framework
open FsUnit
open percolation
open System


[<Test; ExpectedException(typeof<ArgumentException>)>]
let ``createGrid with n <= 0 throws invalid argument`` () =
    createGrid -1 |> ignore


[<TestCase(1)>]
[<TestCase(13)>]
let ``createGrid n creates a nxn grid with closed sites`` n =
    let expectedGrid = Array2D.create n n Closed
    createGrid n |> should equal expectedGrid


[<Test; ExpectedException(typeof<ArgumentOutOfRangeException>)>]
let ``openSite params should be >= 0`` () =
    let grid = createGrid 3
    openSite -1 2 grid |> ignore


[<Test; ExpectedException(typeof<ArgumentOutOfRangeException>)>]
let ``openSite params should be < n`` () =
    let grid = createGrid 3
    openSite 0 3 grid |> ignore


[<Test>]
let ``openSite i,j should open only the [i,j] site `` () =
    let initialGrid = createGrid 2
    let expectedGrid = createGrid 2
    Array2D.set expectedGrid 1 1 Open
    openSite 1 1 initialGrid |> should equal expectedGrid


[<Test>]
let ``isOpen should work`` () =
    let grid = openSite 1 1 (createGrid 2)
    isOpen 1 1 grid |> should be True
    isOpen 0 0 grid |> should be False

