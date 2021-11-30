namespace FRange

open System

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

/// Internal representation of a directed bound within a range.
/// this is used to sort bounds.
type private BoundDir<'t when 't : comparison> =
    {
        /// Bound, if any.
        BoundOpt : Option<Bound<'t>>

        /// -1 -> lower bound, 1 -> upper bound.
        Direction : int
    }

module private BoundDir =

    /// Creates a directed bound.
    let create boundOpt direction =
        if direction <> 1 && direction <> -1 then
            invalidArg (nameof direction) "Invalid direction"
        {
            BoundOpt = boundOpt
            Direction = direction
        }

    /// Compares two directed bounds in the following order:
    /// * An infinite lower bound is less than any other bound.
    /// * An infinite upper bound is more than any other bound.
    /// * Finite bonds are sorted by value.
    /// * An inclusive bound is extends farther in its direction than
    ///   an exclusive bound of the same value.
    /// * An inclusive lower bound is optionally less/more than an exclusive
    ///   upper bound of the same value. This ensures that adjacent ranges
    ///   overlap correctly.
    let compare overlap boundDirA boundDirB =
        if overlap <> 1 && overlap <> -1 then
            invalidArg (nameof overlap) "Invalid overlap"
        let toTuple boundDir =
            match boundDir.BoundOpt with
                | None ->
                    boundDir.Direction, None, 0, boundDir.Direction
                | Some (Inclusive value) ->
                    0,
                    Some value,
                    boundDir.Direction,
                    overlap * boundDir.Direction
                | Some (Exclusive value) ->
                    0,
                    Some value,
                    -boundDir.Direction,
                    overlap * boundDir.Direction
        compare (toTuple boundDirA) (toTuple boundDirB)
