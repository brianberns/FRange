namespace FRange

/// An inclusive or exclusive bound at a specific value.
[<NoComparison>]
type Bound<'t> =

    /// Inclusive bound.
    | Inclusive of 't

    /// Exclusive bound.
    | Exclusive of 't

    /// Bound's value.
    member bound.Value =
        match bound with
            | Inclusive x
            | Exclusive x -> x

module Bound =

    /// Inverts the given bound.
    let inverse = function
        | Inclusive x -> Exclusive x
        | Exclusive x -> Inclusive x

    /// Inverts the given bound.
    let inverseOpt boundOpt =
        boundOpt |> Option.map inverse

/// Lower vs. upper bound.
type private BoundType =
    | Lower = -1
    | Upper = 1

/// Internal representation of a directed bound within a range.
/// this is used to sort bounds.
type private BoundDir<'t when 't : comparison> =
    {
        /// Bound, if any.
        BoundOpt : Option<Bound<'t>>

        /// Lower vs. upper bound.
        Direction : BoundType
    }

module private BoundDir =

    /// Creates a directed bound.
    let create boundOpt direction =
        {
            BoundOpt = boundOpt
            Direction = direction
        }

    /// Sorts directed bounds in the following order:
    /// * An infinite lower bound is less than any other bound.
    /// * An infinite upper bound is more than any other bound.
    /// * Finite bonds are sorted by value.
    /// * An inclusive bound is extends farther in its direction than
    ///   an exclusive bound of the same value.
    let sortProjection tieBreaker boundDir =
        match boundDir.BoundOpt with
            | None ->
                int boundDir.Direction,
                None,
                0,
                tieBreaker
            | Some (Inclusive value) ->
                0,
                Some value,
                int boundDir.Direction,
                tieBreaker
            | Some (Exclusive value) ->
                0,
                Some value,
                -1 * int boundDir.Direction,
                tieBreaker
