namespace FRange

#nowarn "40"   // allow recursive values

open FsCheck
open FsCheck.Xunit

module Int =
    let gen = Arb.from<int>.Generator

type Triplet =
    {
        AOpt : Option<int>
        B : int
        COpt : Option<int>
    }

type Generators =

    static member Range() =
        let genBoundOpt =
            Int.gen
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

    static member Triplet() =
        gen {
            let! aOpt = Int.gen |> Gen.optionOf
            let! b =
                Int.gen
                    |> Gen.where (fun b ->
                        aOpt
                            |> Option.map (fun a -> b > a)
                            |> Option.defaultValue true)
            let! cOpt =
                Int.gen
                    |> Gen.optionOf
                    |> Gen.where (fun cOpt ->
                        cOpt
                            |> Option.map (fun c -> c > b)
                            |> Option.defaultValue true)
            return { AOpt = aOpt; B = b; COpt = cOpt }
        } |> Arb.fromGen

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

    [<Property>]
    let ``Merge adjancent ranges`` triplet =
        let boundAOpt = triplet.AOpt |> Option.map Inclusive
        let boundCOpt = triplet.COpt |> Option.map Inclusive
        let rangeABIncl = Range.create boundAOpt (Some (Inclusive triplet.B))
        let rangeABExcl = Range.create boundAOpt (Some (Exclusive triplet.B))
        let rangeBCIncl = Range.create (Some (Inclusive triplet.B)) boundCOpt
        let rangeBCExcl = Range.create (Some (Exclusive triplet.B)) boundCOpt
        let rangeAC = Range.create boundAOpt boundCOpt
        Range.union2 rangeABIncl rangeBCIncl = [ rangeAC ]
            && Range.union2 rangeABIncl rangeBCExcl = [ rangeAC ]
            && Range.union2 rangeABExcl rangeBCIncl = [ rangeAC ]
            && Range.union2 rangeABExcl rangeBCExcl = [ rangeABExcl; rangeBCExcl ]

    [<assembly: Properties(
        Arbitrary = [| typeof<Generators> |],
        MaxTest = 10000)>]
    do ()
