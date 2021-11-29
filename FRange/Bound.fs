namespace FRange

open System

[<NoComparison>]
type Bound<'t> =
    | Inclusive of 't
    | Exclusive of 't

    member bound.Value =
        match bound with
            | Inclusive x
            | Exclusive x -> x

[<CustomComparison; CustomEquality>]
type private BoundDir<'t when 't : comparison> =
    {
        BoundOpt : Option<Bound<'t>>
        Direction : int
    }

    member this.CompareTo(other) =
        let toTuple boundDir =
            match boundDir.BoundOpt with
                | None -> boundDir.Direction, None, 0, boundDir.Direction
                | Some (Inclusive value) -> 0, Some value,  boundDir.Direction, boundDir.Direction
                | Some (Exclusive value) -> 0, Some value, -boundDir.Direction, boundDir.Direction
        compare (toTuple this) (toTuple other)

    member this.CompareTo(other : obj) =
        (this :> IComparable<BoundDir<'t>>).CompareTo(other :?> BoundDir<'t>)

    override this.Equals(other) =
        this.CompareTo(other) = 0

    override _.GetHashCode() =
        raise <| NotImplementedException()

    interface IComparable<BoundDir<'t>> with
        member this.CompareTo(other) = this.CompareTo(other)

    interface IComparable with
        member this.CompareTo(other) = this.CompareTo(other)

module private BoundDir =

    let create boundOpt direction =
        if direction <> 1 && direction <> -1 then
            invalidArg (nameof direction) "Invalid direction"
        {
            BoundOpt = boundOpt
            Direction = direction
        }
