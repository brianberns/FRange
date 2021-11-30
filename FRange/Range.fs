namespace FRange

/// A range of values, optionally bounded in one or both directions.
/// Examples:
/// * x < 3
/// * 2 < x <= 4
/// * 5 < x
[<NoComparison>]
type Range<'t when 't : comparison> =
    private {

        /// Optional lower bound.
        _LowerOpt : Option<Bound<'t>>

        /// Optional upper bound.
        _UpperOpt : Option<Bound<'t>>
    }

    /// Optional lower bound.
    member range.LowerOpt = range._LowerOpt

    /// Optional upper bound.
    member range.UpperOpt = range._UpperOpt

module Range =

    /// Tries to create a range with the given optional bounds.
    let tryCreate lowerOpt upperOpt =
        let isValid =
            match lowerOpt, upperOpt with
                | Some (Inclusive lower), Some (Inclusive higher) ->
                    lower <= higher   // range includes a single point
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

    /// Creates a range with the given optional bounds. An exception
    /// is thrown if the range is invalid.
    let create lowerOpt upperOpt =
        tryCreate lowerOpt upperOpt
            |> Option.defaultWith (fun () ->
                failwith "Invalid range")

    /// Creates a range that contains a single point.
    let singleton x =
        let bound = Some (Inclusive x)
        create bound bound

    /// A range that's infinite (i.e. unbounded) in both directions.
    let infinite<'t when 't : comparison> : Range<'t> =
        create None None

    /// Indicates whether the given range contains the given point.
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

    /// Extracts directed bounds from the given ranges.
    let private toBoundDirs ranges =
        seq {
            for range in ranges do
                yield BoundDir.create range._LowerOpt -1
                yield BoundDir.create range._UpperOpt  1
        }

    /// Merges the given ranges where possible. The result is a normalized
    /// list of ranges, even if no merges occurred.
    let merge ranges =
        let boundDirs =
            ranges
                |> toBoundDirs
                |> Seq.sortWith (BoundDir.compare 1)
        let activeCount, lowerBoundOpt, outRanges =
            ((0, None, []), boundDirs)
                ||> Seq.fold (fun (activeCount, lowerBoundOpt, outRanges) boundDir ->
                    match boundDir.Direction with

                            // lower bound activates its range
                        | -1 ->
                                // if no ranges currently active, this lower bound starts an output range
                            assert(activeCount >= 0)
                            let lowerBoundOpt' =
                                if activeCount = 0 then boundDir.BoundOpt
                                else lowerBoundOpt
                            activeCount + 1, lowerBoundOpt', outRanges

                            // upper bound deactivates its range
                        |  1 ->
                            assert(activeCount > 0)
                            let activeCount' = activeCount - 1

                                // if no ranges currently active, this upper bound ends an output range
                            if activeCount' = 0 then
                                let range = create lowerBoundOpt boundDir.BoundOpt
                                activeCount', None, range :: outRanges
                            else activeCount', lowerBoundOpt, outRanges

                        |  _ -> failwith "Unexpected")
        assert(activeCount = 0)
        assert(lowerBoundOpt.IsNone)
        List.rev outRanges

    /// Determines the union of the given ranges.
    let union rangesA rangesB =
        Seq.append rangesA rangesB
            |> merge

    /// Determines the intersection of the given ranges.
    let intersection rangesA rangesB =
        let pairs =
            let convert idx ranges =
                ranges
                    |> toBoundDirs
                    |> Seq.map (fun boundDir -> boundDir, idx)
            seq {
                yield! rangesA |> convert 0
                yield! rangesB |> convert 1
            } |> Seq.sortWith (fun (boundDirA, _) (boundDirB , _) ->
                BoundDir.compare -1 boundDirA boundDirB)
        let activeCounts =
            [| 0; 0 |]
                |> System.Collections.Immutable.ImmutableArray.ToImmutableArray
        let activeCounts', lowerBoundOpt, outRanges =
            ((activeCounts, None, []), pairs)
                ||> Seq.fold (fun (activeCounts, lowerBoundOpt, outRanges) (boundDir, idx) ->
                    match boundDir.Direction with

                            // lower bound activates its range
                        | -1 ->
                                // lower bound starts an output range?
                            assert(activeCounts[idx] >= 0)
                            assert(activeCounts[1-idx] >= 0)
                            let lowerBoundOpt' =
                                if (activeCounts[idx] = 0) && (activeCounts[1-idx] > 0) then
                                    boundDir.BoundOpt
                                else lowerBoundOpt

                            let activeCounts' =
                                activeCounts.SetItem(idx, activeCounts[idx] + 1)
                            activeCounts', lowerBoundOpt', outRanges

                            // upper bound deactivates its range
                        |  1 ->
                            assert(activeCounts[idx] > 0)
                            assert(activeCounts[1-idx] >= 0)
                            let activeCounts' = activeCounts.SetItem(idx, activeCounts[idx] - 1)

                                // upper bound ends an output range?
                            if (activeCounts'[idx] = 0) && (activeCounts'[1-idx] > 0) then
                                let range = create lowerBoundOpt boundDir.BoundOpt
                                activeCounts', None, range :: outRanges
                            else activeCounts', lowerBoundOpt, outRanges

                        |  _ -> failwith "Unexpected")
        assert(activeCounts' |> Seq.forall ((=) 0))
        assert(lowerBoundOpt.IsNone)
        List.rev outRanges

[<AutoOpen>]
module RangeOperators =

    /// Creates an inclusive-inclusive range.
    let (+-+) lower upper =
        Range.create (Some (Inclusive lower)) (Some (Inclusive upper))

    /// Creates an exclusive-inclusive range.
    let ( *-+) lower upper =
        Range.create (Some (Exclusive lower)) (Some (Inclusive upper))

    /// Creates an inclusive-inclusive range.
    let (+-*) lower upper =
        Range.create (Some (Inclusive lower)) (Some (Exclusive upper))

    /// Creates an exclusive-exclusive range.
    let ( *-*) lower upper =
        Range.create (Some (Exclusive lower)) (Some (Exclusive upper))

    /// Creates an inclusive-unbounded range.
    let (!+-) lower =
        Range.create (Some (Inclusive lower)) None

    /// Creates an exclusive-unbounded range.
    let (!*-) lower =
        Range.create (Some (Exclusive lower)) None

    /// Creates an unbounded-inclusive range.
    let (!-+) upper =
        Range.create None (Some (Inclusive upper))

    /// Creates an unbounded-exclusive range.
    let (!-*) upper =
        Range.create None (Some (Exclusive upper))
