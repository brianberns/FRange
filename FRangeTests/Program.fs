namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        IntersectionTests.``Intersection is associative``
            !*- 0
            !*- 0
            !-* 0
            |> printfn "%A"
        0
