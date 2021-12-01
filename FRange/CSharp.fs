namespace FRange.CSharp

open System.Runtime.CompilerServices
open FRange

/// Inclusive/exclusive bound type.
type BoundType =

    /// Bound is inclusive. E.g. x <= 3.
    | Inclusive = 1

    /// Bound is exclusive. E.g. x <= 3.
    | Exclusive = 0

module private BoundOpt =

    /// Value of a bound, if any.
    let value boundOpt =
        match boundOpt with
            | Some (bound : Bound<_>) -> bound.Value
            | None -> failwith "No bound"

    /// Type of a bound, if any.
    let boundType boundOpt =
        match boundOpt with
            | Some (Inclusive _) -> BoundType.Inclusive
            | Some (Exclusive _) -> BoundType.Exclusive
            | None -> failwith "No bound"

/// C# support for creating ranges.
[<AbstractClass; Sealed>]
type Range private () =

    /// Creates a bound of the given value and type.
    static let createBound value boundType =
        match boundType with
            | BoundType.Inclusive -> Inclusive value
            | BoundType.Exclusive -> Exclusive value
            | _ -> failwith "Unexpected"

    /// Creates a range with a lower bound, but no upper bound.
    static member CreateLower(value, boundType) =
        let bound = createBound value boundType
        Range.create (Some bound) None

    /// Creates a range with an upper bound, but no lower bound.
    static member CreateUpper(value, boundType) =
        let bound = createBound value boundType
        Range.create None (Some bound)

    /// Creates a range with the given bounds.
    static member Create(lowerValue, lowerBoundType, upperValue, upperBoundType) =
        let lower = createBound lowerValue lowerBoundType
        let upper = createBound upperValue upperBoundType
        Range.create (Some lower) (Some upper)

    /// Creates a range that contains a single value.
    [<Extension>]
    static member Singleton(value) =
        Range.singleton value

    /// A range that's infinite (i.e. unbounded) in both directions.
    static member Infinite() =
        Range.infinite

/// A collection of ranges.
type MultiRange<'t when 't : comparison>(ranges : seq<Range<'t>>) =

    /// Ranges in this collection.
    member _.Ranges = ranges

    /// Display string.
    override _.ToString() =
        $"{ranges}"

    /// Indicates whether the receiver's ranges are equivalent to the
    /// given ranges.
    member _.IsEquivalent(mr : MultiRange<_>) =
        Range.merge ranges = Range.merge mr.Ranges

    /// Merges the receiver's ranges (where possible).
    member _.Merge() =
        Range.merge ranges
            |> MultiRange

    /// Union of receiver's ranges with given ranges.
    member _.Union(mr : MultiRange<_>) =
        Range.union ranges mr.Ranges
            |> MultiRange

    /// Intersection of receiver's ranges with given ranges.
    member _.Intersection(mr : MultiRange<_>) =
        Range.intersection ranges mr.Ranges
            |> MultiRange

    /// Removes everything in the given ranges from the receiver.
    member _.Difference(mr : MultiRange<_>) =
        Range.difference ranges mr.Ranges
            |> MultiRange

    /// Inverts the receiver's ranges.
    member _.Inverse() =
        Range.inverse ranges
            |> MultiRange

/// C# support for creating multi-ranges.
[<AbstractClass; Sealed>]
type MultiRange private () =

    /// Creates a multi-range containing the given ranges.
    static member Create(ranges) =
        MultiRange<_>(ranges)

[<Extension>]
type RangeExt =

    /// Indicates whether this range has a lower bound.
    [<Extension>]
    static member HasLowerBound(range) = range._LowerOpt.IsSome

    /// Value of this range's lower bound, if any.
    [<Extension>]
    static member LowerBoundValue(range) = BoundOpt.value range._LowerOpt

    /// Type of this range's lower bound, if any.
    [<Extension>]
    static member LowerBoundType(range) = BoundOpt.boundType range._LowerOpt

    /// Indicates whether this range has a lower bound.
    [<Extension>]
    static member HasUpperBound(range) = range._UpperOpt.IsSome

    /// Value of this range's upper bound, if any.
    [<Extension>]
    static member UpperBoundValue(range) = BoundOpt.value range._UpperOpt

    /// Type of this range's lower bound, if any.
    [<Extension>]
    static member UpperBoundType(range) = BoundOpt.boundType range._UpperOpt

    /// Indicates whether this range contains the given value.
    [<Extension>]
    static member Contains(range, value) = range |> Range.contains value

    /// Creates a multi-range containing the given range.
    [<Extension>]
    static member ToMultiRange(range) = [range] |> MultiRange<_>
