namespace FRange

open System

module Program =

    [<EntryPoint>] 
    let main args =

        let parse = DateTime.Parse
        let aSchedule =
            [|
                parse "11:30am" +-* parse "1pm"
                parse "2:30pm" +-* parse "4:30pm"
            |]
        let bSchedule =
            [|
                parse "10am" +-* parse "11am"
                parse "11am" +-* parse "3pm"
            |]
        let day = parse "9am" +-* parse "5pm"
        let free =
            Range.difference [day]
                (Range.union aSchedule bSchedule)
        printfn "%A" free

        0
