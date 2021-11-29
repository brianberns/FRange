namespace FRange

[<NoComparison>]
type Bound<'t> =
    | Inclusive of 't
    | Exclusive of 't

    member bound.Value =
        match bound with
            | Inclusive x
            | Exclusive x -> x

[<NoComparison>]
type Range<'t when 't : comparison> =
    private {
        LowerOpt : Option<Bound<'t>>
        UpperOpt : Option<Bound<'t>>
    }

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
            |> Option.defaultWith (fun () ->
                failwith "Invalid range")

    let singleton x =
        let bound = Some (Inclusive x)
        create bound bound

    let infinite<'t when 't : comparison> : Range<'t> =
        create None None

    let contains x range =
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

    let (|Value|) (bound : Bound<_>) =
        bound.Value

    let tryUnion rangeA rangeB =
        let points =
            [|
                (rangeA.LowerOpt, -1, 0)
                (rangeB.LowerOpt, -1, 1)
                (rangeA.UpperOpt,  1, 0)
                (rangeB.UpperOpt,  1, 1)
            |] |> Array.sortBy (fun (boundOpt, dir, _) ->
                match boundOpt with
                    | None -> dir, None, 0
                    | Some (Inclusive value) -> 0, Some value,  dir
                    | Some (Exclusive value) -> 0, Some value, -dir)
        assert(points.Length = 4)
        let lowerOpt, _, owner0 = points[0]
        let _, _, owner1 = points[1]
        if owner0 = owner1 then None
        else
            let upperOpt, _, _ = points[3]
            Some (create lowerOpt upperOpt)

type Range<'t when 't : comparison> with

    member range.LowerBoundOpt = range.LowerOpt
    member range.UpperBoundOpt = range.UpperOpt
