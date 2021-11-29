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
        match range.LowerOpt, range.UpperOpt with
            | Some lower, Some upper ->
                lower.Value <= upper.Value
            | _ -> true

    [<Property>]
    let ``Bounds are in or out of range`` (range : Range<int>) =
        let test = function
            | Some (Inclusive x) -> range |> Range.contains x
            | Some (Exclusive x) -> range |> Range.contains x |> not
            | None -> true
        test range.LowerOpt && test range.UpperOpt

    module Range =

        let union2 rangeA rangeB =
            Range.union [ rangeA; rangeB ]

    [<Property>]
    let ``Union of two ranges contains both ranges`` (rangeA : Range<int>) rangeB =
        let ranges = Range.union2 rangeA rangeB
        let test range =
            Range.union (range :: ranges) = ranges
        test rangeA && test rangeB

    [<Property>]
    let ``Union of range with itself is identity`` (range : Range<int>) =
        Range.union2 range range = [ range ]

    [<Property>]
    let ``Union is commutative`` (rangeA : Range<int>) rangeB =
        Range.union2 rangeA rangeB
            = Range.union2 rangeB rangeA

    [<Property>]
    let ``Union is associative`` (rangeA : Range<int>) rangeB rangeC =
        let rangesAB = Range.union2 rangeA rangeB
        let rangesBC = Range.union2 rangeB rangeC
        let union0 = Range.union (rangesAB @ [ rangeC ])
        let union1 = Range.union (rangeA :: rangesBC)
        union0 = union1

    [<assembly: Properties(
        Arbitrary = [| typeof<Generators> |],
        MaxTest = 10000)>]
    do ()
