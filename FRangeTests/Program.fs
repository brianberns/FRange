namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        let rangeA = Range.create None (Some (Exclusive 0))   // negative nums
        let rangeB = Range.create (Some (Exclusive 0)) None   // positive nums
        let rangeC = Range.create (Some (Inclusive 0)) None   // non-neg nums
        Tests.``Union is associative`` rangeA rangeB rangeC
            |> printfn "%A"
        0
