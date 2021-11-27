namespace FRange

[<NoComparison>]
type Bound<'t> =
    | Inclusive of 't
    | Exclusive of 't

    member bound.Value =
        match bound with
            | Inclusive x
            | Exclusive x -> x

type Range<'t when 't : comparison> =
    private {
        LowerOpt : Option<Bound<'t>>
        UpperOpt : Option<Bound<'t>>
    }

    member range.LowerBoundOpt = range.LowerOpt
    member range.UpperBoundOpt = range.UpperOpt

module Range =

    let tryCreate lowerOpt upperOpt =
        let isValid =
            match lowerOpt, upperOpt with
                | Some (Inclusive lower), Some (Inclusive higher) ->
                    lower <= higher
                | Some (Inclusive lower), Some (Exclusive higher)
                | Some (Exclusive lower), Some (Inclusive higher)
                | Some (Exclusive lower), Some (Exclusive higher) ->
                    lower < higher
                | _ -> true
        if isValid then
            Some {
                LowerOpt = lowerOpt
                UpperOpt = upperOpt
            }
        else None

    let create lowerOpt upperOpt =
        tryCreate lowerOpt upperOpt
            |> Option.defaultWith (failwith "Invalid range")

    let singleton x =
        let bound = Some (Inclusive x)
        create bound bound

    let infinite<'t when 't : comparison> : Range<'t> =
        create None None

    let inRange x range =
        let inRangeLower =
            match range.LowerOpt with
                | Some (Inclusive lower) -> x >= lower
                | Some (Exclusive lower) -> x > lower
                | None -> true
        let inRangeUpper =
            match range.UpperOpt with
                | Some (Inclusive upper) -> x <= upper
                | Some (Exclusive upper) -> x < upper
                | None -> true
        inRangeLower && inRangeUpper
