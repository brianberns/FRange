namespace FRange

open System

/// An inclusive or exclusive bound at a specific value, or the lack
/// of a bound.
[<NoComparison>]
type Bound<'t> =

    /// Inclusive bound.
    | Inclusive of 't

    /// Exclusive bound.
    | Exclusive of 't

    /// Unbounded.
    | Unbounded

module Bound =

    /// Inverts the given bound.
    let inverse = function
        | Inclusive x -> Exclusive x
        | Exclusive x -> Inclusive x
        | Unbounded -> Unbounded

/// Lower vs. upper bound.
type private BoundType =
    | Lower = -1
    | Upper = 1

/// Internal representation of a directed bound within a range.
/// this is used to sort bounds.
type private BoundDir<'t when 't :> IComparable<'t>> =
    {
        /// Bound in this direction..
        Bound: Bound<'t>

        /// Lower vs. upper bound.
        Direction : BoundType
    }

module private BoundDir =

    /// Creates a directed bound.
    let create bound direction =
        {
            Bound = bound
            Direction = direction
        }

    /// Sorts directed bounds in the following order:
    /// * An infinite lower bound is less than any other bound.
    /// * An infinite upper bound is more than any other bound.
    /// * Finite bonds are sorted by value.
    /// * An inclusive bound extends farther in its direction than
    ///   an exclusive bound of the same value.
    /// * The given tie-breaker value.
    let sortProjection tieBreaker boundDir =
        match boundDir.Bound with
            | Unbounded ->
                int boundDir.Direction,
                None,
                0,
                tieBreaker
            | Inclusive value ->
                0,
                Some value,
                int boundDir.Direction,
                tieBreaker
            | Exclusive value ->
                0,
                Some value,
                -1 * int boundDir.Direction,
                tieBreaker
