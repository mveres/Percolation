module percolation

open System

type SiteState = Closed | Open | Full

type Site =
    {
        Id: int
        Size: int
        State: SiteState
    }

    static member create (id,size,state) = { Id = id; Size = size; State = state}
    static member create id = { Id = id; Size = 1; State = Closed}

    static member openSite s = Site.create (s.Id, s.Size, Open)
    static member fillSite s = Site.create (s.Id, s.Size, Full)

    static member isOpen site = site.State = Open
    static member isFull site = site.State = Full


type Grid =
    {
        Sites: array<Site>
        Size: int
    }

    static member create size =
        let length = size * size + 1
        let sites = [|0..length|] |> Array.map Site.create
        Site.create (0, 1, Full) |> Array.set sites 0
        Site.create (length, 1, Open) |>  Array.set sites length
        { Sites = sites; Size = size}

    static member validateCoords i j grid =
        let size = grid.Size
        if i < 1 || j < 1 || i > size || j > size
        then raise (new ArgumentException("Coordinates outside of of bounds"))

    static member toIndex i j grid =
        Grid.validateCoords i j grid
        (i - 1) * grid.Size + j

    static member openSite i j grid =
        let index = Grid.toIndex i j grid
        let sites = grid.Sites
        Site.openSite sites.[index] |> Array.set sites index
        grid

    static member fillSite i j grid =
        let index = Grid.toIndex i j grid
        let sites = grid.Sites
        Site.fillSite sites.[index] |> Array.set sites index
        grid

    static member siteMatches expectedState index grid =
        let site = grid.Sites.[index]
        site.State = expectedState

    static member siteIsOpenOrFull grid index =
        let site = grid.Sites.[index]
        site.State = Open || site.State = Full


let rec getRoot i grid =
    let id = grid.Sites.[i].Id
    if id = i then i else getRoot id grid

let areConnected index1 index2 grid =
    getRoot index1 grid = getRoot index2 grid

let replace r1 r2 grid =
    let s1 = grid.Sites.[r1]
    let s2 = grid.Sites.[r2]
    Array.set grid.Sites r1 (Site.create(r2, s1.Size, s1.State))
    Array.set grid.Sites r2 (Site.create(r2, s1.Size +  s2.Size, s2.State))

let connect grid index1 index2 =
    let r1 = getRoot index1 grid
    let r2 = getRoot index2 grid

    if r1 <> r2 then
        if grid.Sites.[r1].Size < grid.Sites.[r2].Size
        then replace r1 r2 grid
        else replace r2 r1 grid

let openSite i j grid =
    Grid.validateCoords i j grid
    Grid.openSite i j grid |> ignore

    let index = Grid.toIndex i j grid

    let neighbours =
        [(i, j - 1); (i, j + 1); (i - 1, j); (i + 1, j)]
        |> List.filter (fun (i1, j1) -> j > 1 && j1 <= grid.Size)
        |> List.map (fun (i1, j1) -> if i1 = 0 then 0
                                     elif i1 > grid.Size then grid.Sites.Length - 1
                                     else Grid.toIndex i1 j1 grid)
        |> Set.ofList
        |> Set.filter (Grid.siteIsOpenOrFull grid)
        |> Set.iter (connect grid index)

    if areConnected index 0 grid
    then Grid.fillSite i j grid
    else grid

let isOpen i j grid =
    let index = Grid.toIndex i j grid
    Grid.siteMatches Open index grid

let isFull i j grid =
    let index = Grid.toIndex i j grid
    Grid.siteMatches Full index grid