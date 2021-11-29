namespace FRange

#nowarn "40"   // allow recursive values

open FsCheck
open FsCheck.Xunit

type Generators =

    static member Range() =
        let genBoundOpt =
            Arb.from<int>.Generator
                |> Gen.apply (Gen.elements [ Inclusive; Exclusive ])
                |> Gen.optionOf
        let rec genRange =
            gen {
                let! lowerOpt = genBoundOpt
                let! upperOpt = genBoundOpt
                match Range.tryCreate lowerOpt upperOpt with
                    | Some range -> return range
                    | None -> return! genRange
            }
        genRange |> Arb.fromGen

module Tests =

    [<Property>]
    let ``Lower value <= upper value`` (range : Range<int>) =
        match range.LowerBoundOpt, range.UpperBoundOpt with
            | Some lower, Some upper ->
                lower.Value <= upper.Value
            | _ -> true

    [<Property>]
    let ``Bounds are in or out of range`` (range : Range<int>) =
        let test = function
            | Some (Inclusive x) -> range |> Range.contains x
            | Some (Exclusive x) -> range |> Range.contains x |> not
            | None -> true
        test range.LowerBoundOpt && test range.UpperBoundOpt

    [<Property>]
    let ``Union of two ranges contains both ranges`` (rangeA : Range<int>) rangeB =
        match Range.tryUnion rangeA rangeB with
            | Some union ->
                let test range =
                    match Range.tryUnion range union with
                        | Some union' -> union' = union
                        | None -> false
                test rangeA && test rangeB
            | None -> true

    [<Property>]
    let ``Union of range with itself is identity`` (range : Range<int>) =
        Range.tryUnion range range = Some range

    [<Property>]
    let ``Union is commutative`` (rangeA : Range<int>) rangeB =
        Range.tryUnion rangeA rangeB
            = Range.tryUnion rangeB rangeA

    [<Property>]
    let ``Union is associative`` (rangeA : Range<int>) rangeB rangeC =
        match Range.tryUnion rangeA rangeB, Range.tryUnion rangeB rangeC with
            | Some rangeAB, Some rangeBC ->
                match Range.tryUnion rangeAB rangeC, Range.tryUnion rangeA rangeBC with
                    | Some range1, Some range2 -> range1 = range2
                    | _ -> true
            | _ -> true

    [<assembly: Properties(
        Arbitrary = [| typeof<Generators> |],
        MaxTest = 10000)>]
    do ()
