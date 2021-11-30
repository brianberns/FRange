namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        IntersectionTests.``Intersection of ranges is a subset of all ranges`` []
            |> printfn "%A"
        0
