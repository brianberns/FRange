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

    /// Indexes and sorts the bounds of the given ranges.
    let private toIndexedBoundDirs ranges overlap =
        seq {
            for idx, range in Seq.indexed ranges do
                yield idx, BoundDir.create range._LowerOpt -1 overlap
                yield idx, BoundDir.create range._UpperOpt  1 overlap
        } |> Seq.sortBy snd

    /// Computes the union of the given ranges.
    let union ranges =
        let active, lowerBoundOpt, outRanges =
            ((Set.empty, None, []), toIndexedBoundDirs ranges 1)
                ||> Seq.fold (fun (active, lowerBoundOpt, outRanges) (idx, boundDir) ->
                    match boundDir.Direction with

                            // lower bound activates its range
                        | -1 ->
                                // if no ranges currently active, this lower bound starts a new output range
                            let lowerBoundOpt' =
                                if active.IsEmpty then boundDir.BoundOpt
                                else lowerBoundOpt

                            assert(active.Contains(idx) |> not)
                            let active' = active.Add(idx)
                            active', lowerBoundOpt', outRanges

                            // upper bound deactivates its range
                        |  1 ->
                            assert(active.Contains(idx))
                            let active' = active.Remove(idx)

                                // if no ranges currently active, this upper bound ends a new output range
                            if active'.IsEmpty then
                                let range = create lowerBoundOpt boundDir.BoundOpt
                                active', None, range :: outRanges
                            else active', lowerBoundOpt, outRanges

                        |  _ -> failwith "Unexpected")
        assert(active.IsEmpty)
        assert(lowerBoundOpt.IsNone)
        List.rev outRanges

    let intersect ranges =
        let ranges = Seq.toArray ranges
        let active, lowerBoundOpt, outRanges =
            ((Set.empty, None, []), toIndexedBoundDirs ranges -1)
                ||> Seq.fold (fun (active, lowerBoundOpt, outRanges) (idx, boundDir) ->
                    match boundDir.Direction with

                            // lower bound activates its range
                        | -1 ->
                            assert(active.Contains(idx) |> not)
                            let active' = active.Add(idx)

                                // if all ranges currently active, this lower bound starts the output range
                            let lowerBoundOpt' =
                                if active'.Count = ranges.Length then
                                    boundDir.BoundOpt
                                else lowerBoundOpt

                            active', lowerBoundOpt', outRanges

                            // upper bound deactivates its range
                        |  1 ->
                                // if all ranges currently active, this upper bound ends the output range
                            let lowerBoundOpt', outRanges' =
                                if active.Count = ranges.Length then
                                    let range = create lowerBoundOpt boundDir.BoundOpt
                                    None, range :: outRanges
                                else lowerBoundOpt, outRanges

                            assert(active.Contains(idx))
                            active.Remove(idx), lowerBoundOpt', outRanges'

                        |  _ -> failwith "Unexpected")
        assert(active.IsEmpty)
        assert(lowerBoundOpt.IsNone)
        assert(outRanges.Length <= 1)
        outRanges

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
