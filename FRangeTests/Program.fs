namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        [
            Range.create (Some (Inclusive 0)) None
            Range.create None (Some (Exclusive 0))
        ]
            |> Range.union
            |> printfn "%A"
        0
