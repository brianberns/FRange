namespace FRange

module Program =

    [<EntryPoint>] 
    let main args =
        let ranges = [ !*- 0 ; !-* -1 ]
        ``Difference tests``.``Double inverse is identity`` ranges
            |> printfn "%A"
        0
