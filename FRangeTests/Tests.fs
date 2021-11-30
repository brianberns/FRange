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
                |> Gen.apply (Gen.elements [Inclusive; Exclusive])
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
        Range.union [rangeA] [rangeB] = [1 +-* 4]
            && Range.intersection [rangeA] [rangeB] = [2 *-+ 3]

    [<Property>]
    let ``Unbounded operators`` () =
        let rangeA = !-+ 1
        let rangeB = !*- -1
        Range.union [rangeA] [rangeB] = [Range.infinite]
            && Range.intersection [rangeA] [rangeB] = [-1 *-+ 1]

module MergeTests =

    [<Property>]
    let ``Merged ranges are a superset of all ranges`` (ranges : List<Range<int>>) =
        let merged = Range.merge ranges
        ranges
            |> Seq.forall (fun range ->
                Range.merge (range :: merged) = merged)

    [<Property>]
    let ``Merge of no ranges is empty`` () =
        Range.merge [] = []

    [<Property>]
    let ``Merge of range by itself is self`` (range : Range<int>) =
        Range.merge [range] = [range]

module UnionTests =

    [<Property>]
    let ``Union of range with empty is itself`` (range : Range<int>) =
        Range.union [range] [] = [range]

    [<Property>]
    let ``Union of range with itself is itself`` (range : Range<int>) =
        Range.union [range] [range] = [range]

    [<Property>]
    let ``Union of ranges with infinite range is infinite`` (ranges : List<Range<int>>) =
        Range.union ranges [Range.infinite] = [Range.infinite]

    [<Property>]
    let ``Union is commutative``
        (rangesA : List<Range<int>>)
        (rangesB : List<Range<int>>) =
        Range.union rangesA rangesB
            = Range.union rangesB rangesA

    [<Property>]
    let ``Union is associative`` 
        (rangesA : List<Range<int>>)
        (rangesB : List<Range<int>>)
        (rangesC : List<Range<int>>) =
        let rangesAB = Range.union rangesA rangesB
        let rangesBC = Range.union rangesB rangesC
        Range.union rangesAB rangesC =
            Range.union rangesA rangesBC

    [<Property>]
    let ``Union merges adjancent ranges`` triplet =
        let boundAOpt = triplet.AOpt |> Option.map Inclusive
        let boundCOpt = triplet.COpt |> Option.map Inclusive
        let rangeABIncl = Range.create boundAOpt (Some (Inclusive triplet.B))
        let rangeABExcl = Range.create boundAOpt (Some (Exclusive triplet.B))
        let rangeBCIncl = Range.create (Some (Inclusive triplet.B)) boundCOpt
        let rangeBCExcl = Range.create (Some (Exclusive triplet.B)) boundCOpt
        let rangeAC = Range.create boundAOpt boundCOpt
        Range.union [rangeABIncl] [rangeBCIncl] = [rangeAC]
            && Range.union [rangeABIncl] [rangeBCExcl] = [rangeAC]
            && Range.union [rangeABExcl] [rangeBCIncl] = [rangeAC]
            && Range.union [rangeABExcl] [rangeBCExcl] = [rangeABExcl; rangeBCExcl]

module IntersectionTests =

    [<Property>]
    let ``Intersection of ranges is a subset of all ranges`` (ranges : List<Range<int>>) =
        let intersection =
            ranges
                |> Seq.map List.singleton
                |> Seq.reduce Range.intersection
        ranges
            |> Seq.forall (fun range ->
                Range.intersection [range] intersection = intersection)

    [<Property>]
    let ``Intersection of range with empty is itself`` (range : Range<int>) =
        Range.intersection [range] [] = []

    [<Property>]
    let ``Intersection of range with itself is itself`` (range : Range<int>) =
        Range.intersection [range] [range] = [range]

    [<Property>]
    let ``Intersection of ranges with infinite range is identity`` (ranges : List<Range<int>>) =
        Range.intersection ranges [Range.infinite] = ranges

    [<Property>]
    let ``Intersection is commutative``
        (rangesA : List<Range<int>>)
        (rangesB : List<Range<int>>) =
        Range.intersection rangesA rangesB
            = Range.intersection rangesB rangesA

    [<Property>]
    let ``Intersection is associative``
        (rangesA : List<Range<int>>)
        (rangesB : List<Range<int>>)
        (rangesC : List<Range<int>>) =
        let rangesAB = Range.intersection rangesA rangesB
        let rangesBC = Range.intersection rangesB rangesC
        Range.intersection rangesAB rangesC =
            Range.intersection rangesA rangesBC

    [<Property>]
    let ``Intersection of adjancent ranges is a point at most`` triplet =
        let boundAOpt = triplet.AOpt |> Option.map Inclusive
        let boundCOpt = triplet.COpt |> Option.map Inclusive
        let rangeABIncl = Range.create boundAOpt (Some (Inclusive triplet.B))
        let rangeABExcl = Range.create boundAOpt (Some (Exclusive triplet.B))
        let rangeBCIncl = Range.create (Some (Inclusive triplet.B)) boundCOpt
        let rangeBCExcl = Range.create (Some (Exclusive triplet.B)) boundCOpt
        let rangeB = Range.singleton triplet.B
        Range.intersection [rangeABIncl] [rangeBCIncl] = [rangeB]
            && Range.intersection [rangeABIncl] [rangeBCExcl] = []
            && Range.intersection [rangeABExcl] [rangeBCIncl] = []
            && Range.intersection [rangeABExcl] [rangeBCExcl] = []
