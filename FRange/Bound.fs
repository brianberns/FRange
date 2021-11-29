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
[<CustomComparison; CustomEquality>]
type private BoundDir<'t when 't : comparison> =
    {
        /// Bound, if any.
        BoundOpt : Option<Bound<'t>>

        /// -1 -> lower bound, 1 -> upper bound.
        Direction : int

        /// Inclusive lower bound is less/more than an exclusive upper bound
        /// of the same value:
        /// *  1 -> less than
        /// * -1 -> more than
        Overlap : int
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
    member this.CompareTo(other) =
        let toTuple boundDir =
            match boundDir.BoundOpt with
                | None ->
                    boundDir.Direction, None, 0, boundDir.Direction
                | Some (Inclusive value) ->
                    0,
                    Some value,
                    boundDir.Direction,
                    boundDir.Overlap * boundDir.Direction
                | Some (Exclusive value) ->
                    0,
                    Some value,
                    -boundDir.Direction,
                    boundDir.Overlap * boundDir.Direction
        compare (toTuple this) (toTuple other)

    /// Compares two bounds.
    member this.CompareTo(other : obj) =
        (this :> IComparable<BoundDir<'t>>).CompareTo(other :?> BoundDir<'t>)

    /// Boilerplate required by F#/.NET.
    override this.Equals(other) =
        this.CompareTo(other) = 0

    /// Boilerplate required by F#/.NET.
    override _.GetHashCode() =
        raise <| NotImplementedException()

    /// Boilerplate required by F#/.NET.
    interface IComparable<BoundDir<'t>> with
        member this.CompareTo(other) = this.CompareTo(other)

    /// Boilerplate required by F#/.NET.
    interface IComparable with
        member this.CompareTo(other) = this.CompareTo(other)

module private BoundDir =

    /// Creates a directed bound.
    let create boundOpt direction overlap =
        if direction <> 1 && direction <> -1 then
            invalidArg (nameof direction) "Invalid direction"
        if overlap <> 1 && overlap <> -1 then
            invalidArg (nameof overlap) "Invalid overlap"
        {
            BoundOpt = boundOpt
            Direction = direction
            Overlap = overlap
        }
