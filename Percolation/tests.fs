module tests

open NUnit.Framework
open FsUnit
open percolation
open System


[<Test>]
let ``Create 2 x 2 percolation grid creates 0,,5 id array`` ()=
    let expected = [|(0, Full); (1, Closed); (2, Closed); (3, Closed); (4, Closed); (5, Open)|]
    let model = createModel 2
    model |> fst |> should equal expected
    model |> snd |> should equal 2


[<TestCase(0, 0)>]
[<TestCase(-1, 1)>]
[<TestCase(1, -1)>]
[<TestCase(2, 3)>]
let ``Given invalid params When open is called Then it throws argument exception`` i j =
    (fun () -> openSite i j (createModel 2)|> ignore)
    |> should throw typeof<ArgumentException>


[<Test>]
let ``Given 2 x 2 percolation grid When opening (2,1) Then (2,1) is open`` () =
    let model = openSite 2 1 (createModel 2)
    isOpen 2 1 model |> should be True


[<Test>]
let ``Given 2 x 2 percolation grid When opening (1,2) Then (1,2) is full`` () =
    let model = openSite 1 2 (createModel 2)
    isFull 1 2 model |>  should be True