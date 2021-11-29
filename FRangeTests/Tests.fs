namespace FRange

#nowarn "40"   // allow recursive values

open FsCheck
open FsCheck.Xunit

module Int =
    let gen = Arb.from<int>.Generator

module Range =

    let arb =
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

    let union2 rangeA rangeB =
        Range.union [ rangeA; rangeB ]

type Triplet =
    {
        AOpt : Option<int>
        B : int
        COpt : Option<int>
    }

    static member Arb =
        gen {
            let! aOpt =
                Int.gen |> Gen.optionOf
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

type Generators =
    static member Range() = Range.arb
    static member Triplet() = Triplet.Arb

module Generators =

    [<assembly: Properties(
        Arbitrary = [| typeof<Generators> |],
        MaxTest = 1000)>]
    do ()

module RangeTests =

    type Generators =
        static member Range() = Range.arb

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

    [<Property>]
    let ``Bounded operators`` () =
        let rangeA = 1 +-+ 3
        let rangeB = 2 *-* 4
        let rangeC = 1 +-* 4
        Range.union2 rangeA rangeB = [ rangeC ]

    (*
    [<Property>]
    let ``Unbounded operators`` () =
        let rangeA = !-+ 1
        let rangeB = !*- -1
        let rangeC = -1 *-+ 1
        Range.intersect2 rangeA rangeB = [ rangeC ]
    *)

module UnionTests =

    [<Property>]
    let ``Union of ranges is a superset of all ranges`` (ranges : List<Range<int>>) =
        let union = Range.union ranges
        ranges
            |> Seq.forall (fun range ->
                Range.union (range :: union) = union)

    [<Property>]
    let ``Union of no ranges is empty`` () =
        Range.union [] = []

    [<Property>]
    let ``Union of range by itself is self`` (range : Range<int>) =
        Range.union [ range ] = [ range ]

    [<Property>]
    let ``Union of range with itself is self`` (range : Range<int>) =
        Range.union2 range range = [ range ]

    [<Property>]
    let ``Union of range with infinite range is infinite`` (range : Range<int>) =
        Range.union2 range Range.infinite = [ Range.infinite ]

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
    let ``Union merges adjancent ranges`` triplet =
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
