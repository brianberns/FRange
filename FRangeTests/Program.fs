namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        let rangeA = 1 +-+ 3
        let rangeB = 2 *-* 4
        Range.difference [rangeA] [rangeB]
            |> printfn "%A"
        0
