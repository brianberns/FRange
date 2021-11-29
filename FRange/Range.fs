﻿namespace FRange

[<NoComparison>]
type Range<'t when 't : comparison> =
    private {
        _LowerOpt : Option<Bound<'t>>
        _UpperOpt : Option<Bound<'t>>
    }

    member range.LowerOpt = range._LowerOpt
    member range.UpperOpt = range._UpperOpt

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
                _LowerOpt = lowerOpt
                _UpperOpt = upperOpt
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
            match range._LowerOpt with
                | Some (Inclusive lower) -> x >= lower
                | Some (Exclusive lower) -> x > lower
                | None -> true
        let inRangeUpper =
            match range._UpperOpt with
                | Some (Inclusive upper) -> x <= upper
                | Some (Exclusive upper) -> x < upper
                | None -> true
        inRangeLower && inRangeUpper

    let tryUnion rangeA rangeB =
        let points =
            [|
                (rangeA._LowerOpt, -1, 0)
                (rangeB._LowerOpt, -1, 1)
                (rangeA._UpperOpt,  1, 0)
                (rangeB._UpperOpt,  1, 1)
            |]
                |> Seq.map (fun (boundOpt, dir, owner) ->
                    (BoundDir.create boundOpt dir), owner)
                |> Seq.sortBy fst
                |> Seq.toArray
        assert(points.Length = 4)
        let lowerBoundDir, owner0 = points[0]
        assert(lowerBoundDir.Direction = -1)
        let _, owner1 = points[1]
        if owner0 = owner1 then None
        else
            let upperBoundDir, _ = points[3]
            assert(upperBoundDir.Direction = 1)
            Some (create lowerBoundDir.BoundOpt upperBoundDir.BoundOpt)

    let private toBoundDirs range =
        seq {
            range._LowerOpt, -1
            range._UpperOpt,  1
        } |> Seq.map (fun (boundOpt, dir) ->
            BoundDir.create boundOpt dir)

    let union ranges =
        let pairs =
            ranges
                |> Seq.indexed
                |> Seq.collect (fun (idx, range) ->
                    toBoundDirs range
                        |> Seq.map (fun boundDir -> idx, boundDir))
                |> Seq.sortBy snd
        let active, lowerBoundOpt, outRanges =
            ((Set.empty, None, []), pairs)
                ||> Seq.fold (fun (active, lowerBoundOpt, outRanges) (idx, boundDir) ->
                    let active', lowerBoundOpt', outRanges' =
                        match boundDir.Direction with
                            | -1 ->
                                assert(active.Contains(idx) |> not)
                                let active' = active.Add(idx)
                                let lowerBoundOpt' =
                                    lowerBoundOpt
                                        |> Option.orElse boundDir.BoundOpt
                                active', lowerBoundOpt', outRanges
                            |  1 ->
                                let active' = active.Remove(idx)
                                let lowerBoundOpt', outRanges' =
                                    if active'.IsEmpty then
                                        let range = create lowerBoundOpt boundDir.BoundOpt
                                        None, range :: outRanges
                                    else lowerBoundOpt, outRanges
                                active', lowerBoundOpt', outRanges'
                            |  _ -> failwith "Unexpected"
                    active', lowerBoundOpt', outRanges')
        assert(active.IsEmpty)
        assert(lowerBoundOpt.IsNone)
        outRanges |> List.rev
