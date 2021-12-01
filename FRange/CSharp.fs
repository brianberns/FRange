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

[<Extension>]
type RangeExt =

    /// Indicates whether this range has a lower bound.
    [<Extension>]
    static member HasLowerBound(range) = range._LowerOpt.IsSome

    /// Value of this range's lower bound, if any.
    [<Extension>]
    static member owerBoundValue(range) = BoundOpt.value range._LowerOpt

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
