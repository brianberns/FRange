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

        /// Lower bound.
        _Lower : Bound<'t>

        /// Upper bound.
        _Upper : Bound<'t>
    }

    /// Display string.
    override range.ToString() =
        let sLower =
            match range._Lower with
                | Inclusive lower -> $"{lower} <= "
                | Exclusive lower -> $"{lower} < "
                | Unbounded -> ""
        let sUpper =
            match range._Upper with
                | Inclusive upper -> $" <= {upper}"
                | Exclusive upper -> $" < {upper}"
                | Unbounded -> ""
        if range._Lower = Unbounded && range._Upper = Unbounded then
            "infinite"
        else
            $"{sLower}x{sUpper}"

    /// Lower bound.
    member range.Lower = range._Lower

    /// Upper bound.
    member range.Upper = range._Upper

module Range =

    /// Tries to create a range with the given bounds.
    let tryCreate lower upper =
        let isValid =
            match lower, upper with
                | Inclusive lower, Inclusive higher ->
                    lower <= higher   // range contains a single value
                | Inclusive lower, Exclusive higher
                | Exclusive lower, Inclusive higher
                | Exclusive lower, Exclusive higher ->
                    lower < higher
                | _ -> true
        if isValid then
            Some {
                _Lower = lower
                _Upper = upper
            }
        else None

    /// Creates a range with the given bounds. An exception is thrown
    /// if the range is invalid.
    let create lower upper =
        tryCreate lower upper
            |> Option.defaultWith (fun () ->
                failwith "Invalid range")

    /// Creates a range that contains a single value.
    let singleton x =
        let bound = Inclusive x
        create bound bound

    /// A range that's infinite (i.e. unbounded) in both directions.
    let infinite<'t when 't : comparison> : Range<'t> =
        create Unbounded Unbounded

    /// Indicates whether the given range contains the given value.
    let contains x range =
        let inRangeLower =
            match range._Lower with
                | Inclusive lower -> x >= lower
                | Exclusive lower -> x > lower
                | Unbounded -> true
        let inRangeUpper =
            lazy
                match range._Upper with
                    | Inclusive upper -> x <= upper
                    | Exclusive upper -> x < upper
                    | Unbounded -> true
        inRangeLower && inRangeUpper.Value   // laziness allows short-circuit

    /// Extracts directed bounds from the given ranges.
    let private toBoundDirs ranges =
        seq {
            for range in ranges do
                yield BoundDir.create
                    range._Lower BoundType.Lower
                yield BoundDir.create
                    range._Upper BoundType.Upper
        }

    /// Merges the given ranges where possible. The result is a normalized
    /// list of ranges, even if no merges occurred.
    let merge ranges =

            // sort bounds so that an {in,ex}clusive upper bound overlaps
            // (and thus merges with) an {ex,in}clusive lower bound of the
            // same value
        let boundDirs =
            ranges
                |> toBoundDirs
                |> Seq.sortBy (fun boundDir ->
                    boundDir |> BoundDir.sortProjection boundDir.Direction)

        let activeCount, lowerBound, outRanges =
            ((0, Unbounded, []), boundDirs)
                ||> Seq.fold (fun (activeCount, lowerBound, outRanges) boundDir ->
                    match boundDir.Direction with

                            // lower bound activates its range
                        | BoundType.Lower ->

                                // if no ranges currently active, this lower bound starts an output range
                            assert(activeCount >= 0)
                            let lowerBound' =
                                if activeCount = 0 then boundDir.Bound
                                else lowerBound
                            activeCount + 1, lowerBound', outRanges

                            // upper bound deactivates its range
                        | BoundType.Upper ->
                            assert(activeCount > 0)
                            let activeCount' = activeCount - 1

                                // if no ranges currently active, this upper bound ends an output range
                            if activeCount' = 0 then
                                let range = create lowerBound boundDir.Bound
                                activeCount', Unbounded, range :: outRanges
                            else activeCount', lowerBound, outRanges

                        |  _ -> failwith "Unexpected bound direction")
        assert(activeCount = 0)
        assert(lowerBound = Unbounded)
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

            // sort bounds so that an {in,ex}clusive upper bound doesn't
            // overlap (and thus doesn't intersect with) an {ex,in}clusive
            // lower bound of the same value
        let pairs =
            toIndexedBoundDirs rangesA rangesB
                |> Seq.sortBy (fun (boundDir, _) ->
                    boundDir
                        |> BoundDir.sortProjection (-1 * int boundDir.Direction))

        let activeCounts', lowerBound, outRanges =
            ((ImmutableArray.ToImmutableArray [| 0; 0 |], Unbounded, []), pairs)
                ||> Seq.fold (fun (activeCounts, lowerBound, outRanges) (boundDir, idx) ->
                    assert((idx = 0) || (idx = 1))
                    match boundDir.Direction with

                            // lower bound activates its range
                        | BoundType.Lower ->
                                // lower bound starts an output range?
                            assert(activeCounts.[idx] >= 0)
                            assert(activeCounts.[1-idx] >= 0)
                            let lowerBound' =
                                if (activeCounts.[idx] = 0) && (activeCounts.[1-idx] > 0) then
                                    boundDir.Bound
                                else lowerBound

                            let activeCounts' =
                                activeCounts.SetItem(idx, activeCounts.[idx] + 1)
                            activeCounts', lowerBound', outRanges

                            // upper bound deactivates its range
                        | BoundType.Upper ->
                            assert(activeCounts.[idx] > 0)
                            assert(activeCounts.[1-idx] >= 0)
                            let activeCounts' = activeCounts.SetItem(idx, activeCounts.[idx] - 1)

                                // upper bound ends an output range?
                            if (activeCounts'.[idx] = 0) && (activeCounts'.[1-idx] > 0) then
                                let range = create lowerBound boundDir.Bound
                                activeCounts', Unbounded, range :: outRanges
                            else activeCounts', lowerBound, outRanges

                        |  _ -> failwith "Unexpected bound direction")
        assert(activeCounts' |> Seq.forall ((=) 0))
        assert(lowerBound = Unbounded)
        List.rev outRanges

    /// Determines the difference of the given ranges by removing
    /// everything in the second sequence from the first sequence.
    let difference rangesA rangesB =
        
            // sort bounds so that b-bounds cancel out matching a-bounds
        let pairs =
            toIndexedBoundDirs rangesA rangesB
                |> Seq.sortBy (fun (boundDir, idx) ->
                    boundDir
                        |> BoundDir.sortProjection (idx * int boundDir.Direction))

        let activeCounts', lowerBound, outRanges =
            ((ImmutableArray.ToImmutableArray [| 0; 0 |], Unbounded, []), pairs)
                ||> Seq.fold (fun (activeCounts, lowerBound, outRanges) (boundDir, idx) ->
                    assert((idx = 0) || (idx = 1))

                    let activeCounts' =
                        activeCounts.SetItem(
                            idx,
                            activeCounts.[idx] - int boundDir.Direction)
                    assert(activeCounts' |> Seq.forall (fun count -> count >= 0))

                    let lowerBound', finished =
                        match idx, boundDir.Direction with

                                // start new range?
                            | 0, BoundType.Lower when activeCounts'.[0] = 1
                                && activeCounts.[1] = 0 ->
                                boundDir.Bound, false
                            | 1, BoundType.Upper when activeCounts'.[1] = 0
                                && activeCounts'.[0] > 0 ->
                                Bound.inverse boundDir.Bound, false

                                // finish new range?
                            | 1, BoundType.Lower when activeCounts'.[1] = 1
                                && activeCounts'.[0] > 0 -> Unbounded, true
                            | 0, BoundType.Upper when activeCounts'.[0] = 0
                                && activeCounts.[1] = 0 -> Unbounded, true

                            | _ -> lowerBound, false

                    let outRanges' =
                        if finished then
                            let upperBound =
                                if idx = 1 then
                                    Bound.inverse boundDir.Bound
                                else boundDir.Bound
                            let range = create lowerBound upperBound
                            range :: outRanges
                        else outRanges

                    activeCounts', lowerBound', outRanges')
        assert(activeCounts' |> Seq.forall ((=) 0))
        assert(lowerBound = Unbounded)
        List.rev outRanges

    /// Inverts the given ranges.
    let inverse ranges =
        difference [infinite] ranges

[<AutoOpen>]
module RangeOperators =

    /// Creates an inclusive-inclusive range.
    let (+-+) lower upper =
        Range.create (Inclusive lower) (Inclusive upper)

    /// Creates an exclusive-inclusive range.
    let ( *-+) lower upper =
        Range.create (Exclusive lower) (Inclusive upper)

    /// Creates an inclusive-inclusive range.
    let (+-*) lower upper =
        Range.create (Inclusive lower) (Exclusive upper)

    /// Creates an exclusive-exclusive range.
    let ( *-*) lower upper =
        Range.create (Exclusive lower) (Exclusive upper)

    /// Creates an inclusive-unbounded range.
    let (!+-) lower =
        Range.create (Inclusive lower) Unbounded

    /// Creates an exclusive-unbounded range.
    let (!*-) lower =
        Range.create (Exclusive lower) Unbounded

    /// Creates an unbounded-inclusive range.
    let (!-+) upper =
        Range.create Unbounded (Inclusive upper)

    /// Creates an unbounded-exclusive range.
    let (!-*) upper =
        Range.create Unbounded (Exclusive upper)
