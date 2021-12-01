namespace FRange

/// Inclusive/exclusive bound type.
type BoundType =

    /// Bound is inclusive. E.g. x <= 3.
    | Inclusive = 1

    /// Bound is exclusive. E.g. x <= 3.
    | Exclusive = 0

module private BoundExt =

    type Bound<'t> with

        /// Bound's type.
        member bound.Type =
            match bound with
                | Inclusive _ -> BoundType.Inclusive
                | Exclusive _ -> BoundType.Exclusive

    /// Value of a bound, if any.
    let value boundOpt =
        match boundOpt with
            | Some (bound : Bound<_>) -> bound.Value
            | None -> failwith "No bound"

    /// Type of a bound, if any.
    let boundType boundOpt =
        match boundOpt with
            | Some (bound : Bound<_>) -> bound.Type
            | None -> failwith "No bound"

/// C# support.
module CSharp =

    type Range<'t when 't : comparison> with

        /// Indicates whether this range has a lower bound.
        member range.HasLowerBound = range._LowerOpt.IsSome

        /// Value of this range's lower bound, if any.
        member range.LowerBoundValue = BoundExt.value range._LowerOpt

        /// Type of this range's lower bound, if any.
        member range.LowerBoundType = BoundExt.boundType range._LowerOpt

        /// Indicates whether this range has a lower bound.
        member range.HasUpperBound = range._UpperOpt.IsSome

        /// Value of this range's upper bound, if any.
        member range.UpperBoundValue = BoundExt.value range._UpperOpt

        /// Type of this range's lower bound, if any.
        member range.UpperBoundType = BoundExt.boundType range._UpperOpt
