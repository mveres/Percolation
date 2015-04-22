module tests

open NUnit.Framework
open FsUnit
open percolation
open System


[<Test>]
let ``Create 2 x 2 percolation grid creates 0,,5 id array`` ()=
    let expected =
        [|
          Site.create(0, 1, Full);
          Site.create(1, 1, Closed);
          Site.create(2, 1, Closed);
          Site.create(3, 1, Closed);
          Site.create(4, 1, Closed);
          Site.create(5, 1, Open)
        |]
    let grid = Grid.create 2
    grid.Sites |> should equal expected
    grid.Size |> should equal 2


[<TestCase(0, 0)>]
[<TestCase(-1, 1)>]
[<TestCase(1, -1)>]
[<TestCase(2, 3)>]
let ``Given invalid params When open is called Then it throws argument exception`` i j =
    (fun () -> Grid.openSite i j (Grid.create 2)|> ignore)
    |> should throw typeof<ArgumentException>


[<Test>]
let ``Given 2 x 2 percolation grid When opening (2,1) Then (2,1) is open`` () =
    let grid = openSite 2 1 (Grid.create 2)
    isOpen 2 1 grid |> should be True


[<Test>]
let ``Given 2 x 2 percolation grid When opening (1,2) Then (1,2) is full`` () =
    let grid = openSite 1 2 (Grid.create 2)
    isFull 1 2 grid |>  should be True

[<Test>]
let ``Given 2 x 2 percolation grid When opening (1,2) and (2,2) Then (2,2) is full`` () =
    let grid1 = openSite 1 2 (Grid.create 2)
    let grid2 = openSite 2 2 grid1
    isFull 2 2 grid2 |>  should be True