namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        [
            Range.create (Some (Inclusive 1)) (Some (Inclusive 2))
            Range.create (Some (Inclusive 3)) (Some (Inclusive 4))
        ]
            |> Range.union
            |> printfn "%A"
        0
