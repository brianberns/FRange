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

[<NoComparison>]
type Range<'t when 't : comparison> =
    private {
        LowerOpt : Option<Bound<'t>>
        UpperOpt : Option<Bound<'t>>
    }

[<CustomComparison; CustomEquality>]
type private BoundDir<'t when 't : comparison> =
    {
        BoundOpt : Option<Bound<'t>>
        Direction : int
    }

    member this.CompareTo(other) =
        let toTuple boundDir =
            match boundDir.BoundOpt with
                | None -> boundDir.Direction, None, 0
                | Some (Inclusive value) -> 0, Some value,  boundDir.Direction
                | Some (Exclusive value) -> 0, Some value, -boundDir.Direction
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
        assert(direction = 1 || direction = -1)
        {
            BoundOpt = boundOpt
            Direction = direction
        }

module Range =

    let tryCreate lowerOpt upperOpt =
        let isValid =
            match lowerOpt, upperOpt with
                | Some (Inclusive lower), Some (Inclusive higher) ->
                    lower <= higher
                | Some (Inclusive lower), Some (Exclusive higher)
                | Some (Exclusive lower), Some (Inclusive higher)
                | Some (Exclusive lower), Some (Exclusive higher) ->
                    lower < higher
                | _ -> true
        if isValid then
            Some {
                LowerOpt = lowerOpt
                UpperOpt = upperOpt
            }
        else None

    let create lowerOpt upperOpt =
        tryCreate lowerOpt upperOpt
            |> Option.defaultWith (fun () ->
                failwith "Invalid range")

    let singleton x =
        let bound = Some (Inclusive x)
        create bound bound

    let infinite<'t when 't : comparison> : Range<'t> =
        create None None

    let contains x range =
        let inRangeLower =
            match range.LowerOpt with
                | Some (Inclusive lower) -> x >= lower
                | Some (Exclusive lower) -> x > lower
                | None -> true
        let inRangeUpper =
            match range.UpperOpt with
                | Some (Inclusive upper) -> x <= upper
                | Some (Exclusive upper) -> x < upper
                | None -> true
        inRangeLower && inRangeUpper

    let (|Value|) (bound : Bound<_>) =
        bound.Value

    let tryUnion rangeA rangeB =
        let points =
            [|
                (rangeA.LowerOpt, -1, 0)
                (rangeB.LowerOpt, -1, 1)
                (rangeA.UpperOpt,  1, 0)
                (rangeB.UpperOpt,  1, 1)
            |]
                |> Seq.map (fun (boundOpt, dir, owner) ->
                    (BoundDir.create boundOpt dir), owner)
                |> Seq.sortBy fst
                |> Seq.toArray
        assert(points.Length = 4)
        let lowerBoundDir, owner0 = points[0]
        assert(lowerBoundDir.Direction = -1)
        let _, owner1 = points[1]
        if owner0 = owner1 then None
        else
            let upperBoundDir, _ = points[3]
            assert(upperBoundDir.Direction = 1)
            Some (create lowerBoundDir.BoundOpt upperBoundDir.BoundOpt)

type Range<'t when 't : comparison> with

    member range.LowerBoundOpt = range.LowerOpt
    member range.UpperBoundOpt = range.UpperOpt
