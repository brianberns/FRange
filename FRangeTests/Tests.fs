namespace FRange

#nowarn "40"   // allow recursive values

open FsCheck
open FsCheck.Xunit

type UnequalPair<'t> =
    MkUnequalPair of 't * 't

type SpanContains =
    MkSpanContains of (int * int) * int

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
            | Some (Inclusive x) -> range |> Range.inRange x
            | Some (Exclusive x) -> range |> Range.inRange x |> not
            | None -> true
        test range.LowerBoundOpt && test range.UpperBoundOpt

    [<assembly: Properties(Arbitrary = [| typeof<Generators> |], MaxTest = 10000)>]
    do ()
