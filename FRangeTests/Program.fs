namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        let rangeA = 0 +-+ 1
        let rangeB = 1 +-+ 2
        Range.intersect2 rangeA rangeB
            |> printfn "%A"
        0
