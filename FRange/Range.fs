namespace FRange

open System.Collections.Immutable

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

    /// Display string.
    override range.ToString() =
        let sLower =
            match range._LowerOpt with
                | Some (Inclusive lower) -> $"{lower} <= "
                | Some (Exclusive lower) -> $"{lower} < "
                | None -> ""
        let sUpper =
            match range._UpperOpt with
                | Some (Inclusive upper) -> $" <= {upper}"
                | Some (Exclusive upper) -> $" < {upper}"
                | None -> ""
        if range._LowerOpt.IsNone && range._UpperOpt.IsNone then
            "infinite"
        else
            $"{sLower}x{sUpper}"

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
                    lower <= higher   // range includes a single value
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

    /// Creates a range that contains a single value.
    let singleton x =
        let bound = Some (Inclusive x)
        create bound bound

    /// A range that's infinite (i.e. unbounded) in both directions.
    let infinite<'t when 't : comparison> : Range<'t> =
        create None None

    /// Indicates whether the given range contains the given value.
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

    /// Maps the given function over the given range.
    let map f range =
        let mapper = f |> Bound.map |> Option.map
        create
            (mapper range._LowerOpt)
            (mapper range._UpperOpt)

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
                |> Seq.sortBy (fun boundDir ->
                    boundDir |> BoundDir.sortProjection boundDir.Direction)
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

    /// Indexes directed bounds from the given ranges.
    let private toIndexedBoundDirs rangesA rangesB =
        let convert idx ranges =
            ranges
                |> toBoundDirs
                |> Seq.map (fun boundDir -> boundDir, idx)
        seq {
            yield! rangesA |> convert 0
            yield! rangesB |> convert 1
        }

    /// Determines the intersection of the given ranges.
    let intersection rangesA rangesB =
        let pairs =
            toIndexedBoundDirs rangesA rangesB
                |> Seq.sortBy (fun (boundDir, _) ->
                    boundDir
                        |> BoundDir.sortProjection -boundDir.Direction)
        let activeCounts', lowerBoundOpt, outRanges =
            ((ImmutableArray.ToImmutableArray [| 0; 0 |], None, []), pairs)
                ||> Seq.fold (fun (activeCounts, lowerBoundOpt, outRanges) (boundDir, idx) ->
                    match boundDir.Direction with

                            // lower bound activates its range
                        | -1 ->
                                // lower bound starts an output range?
                            assert(activeCounts.[idx] >= 0)
                            assert(activeCounts.[1-idx] >= 0)
                            let lowerBoundOpt' =
                                if (activeCounts.[idx] = 0) && (activeCounts.[1-idx] > 0) then
                                    boundDir.BoundOpt
                                else lowerBoundOpt

                            let activeCounts' =
                                activeCounts.SetItem(idx, activeCounts.[idx] + 1)
                            activeCounts', lowerBoundOpt', outRanges

                            // upper bound deactivates its range
                        |  1 ->
                            assert(activeCounts.[idx] > 0)
                            assert(activeCounts.[1-idx] >= 0)
                            let activeCounts' = activeCounts.SetItem(idx, activeCounts.[idx] - 1)

                                // upper bound ends an output range?
                            if (activeCounts'.[idx] = 0) && (activeCounts'.[1-idx] > 0) then
                                let range = create lowerBoundOpt boundDir.BoundOpt
                                activeCounts', None, range :: outRanges
                            else activeCounts', lowerBoundOpt, outRanges

                        |  _ -> failwith "Unexpected")
        assert(activeCounts' |> Seq.forall ((=) 0))
        assert(lowerBoundOpt.IsNone)
        List.rev outRanges

    /// Determines the difference of the given ranges by removing
    /// everything in the second sequence from the first sequence.
    let difference rangesA rangesB =
        let pairs =
            toIndexedBoundDirs rangesA rangesB
                |> Seq.sortBy (fun (boundDir, idx) ->
                    boundDir
                        |> BoundDir.sortProjection (idx * boundDir.Direction))
        let activeCounts', lowerBoundOpt, outRanges =
            ((ImmutableArray.ToImmutableArray [| 0; 0 |], None, []), pairs)
                ||> Seq.fold (fun (activeCounts, lowerBoundOpt, outRanges) (boundDir, idx) ->

                    assert(abs boundDir.Direction = 1)
                    let activeCounts' =
                        activeCounts.SetItem(idx, activeCounts.[idx] - boundDir.Direction)
                    assert(activeCounts' |> Seq.forall (fun count -> count >= 0))

                    let lowerBoundOpt', finished =
                        match idx, boundDir.Direction with

                                // start new range?
                            | 0, -1 when activeCounts'.[0] = 1
                                && activeCounts.[1] = 0 ->
                                boundDir.BoundOpt, false
                            | 1, 1 when activeCounts'.[1] = 0
                                && activeCounts'.[0] > 0 ->
                                Bound.inverseOpt boundDir.BoundOpt, false

                                // finish new range?
                            | 1, -1 when activeCounts'.[1] = 1
                                && activeCounts'.[0] > 0 -> None, true
                            | 0, 1 when activeCounts'.[0] = 0
                                && activeCounts.[1] = 0 -> None, true

                            | _ -> lowerBoundOpt, false

                    let outRanges' =
                        if finished then
                            let upperBoundOpt =
                                if idx = 1 then
                                    Bound.inverseOpt boundDir.BoundOpt
                                else boundDir.BoundOpt
                            let range = create lowerBoundOpt upperBoundOpt
                            range :: outRanges
                        else outRanges

                    activeCounts', lowerBoundOpt', outRanges')
        assert(activeCounts' |> Seq.forall ((=) 0))
        assert(lowerBoundOpt.IsNone)
        List.rev outRanges

    /// Inverts the given ranges.
    let inverse ranges =
        difference [infinite] ranges

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
