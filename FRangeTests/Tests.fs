namespace FRange

#nowarn "40"   // allow recursive values

open System

open FsCheck
open FsCheck.Xunit

module Generator =
    let from<'t> = Arb.from<'t>.Generator   // is there a better way to get this?

module Range =

    let arb<'t when 't : comparison and 't :> IComparable<'t>> =
        let genBound =
            Gen.oneof [
                Generator.from<'t> |> Gen.map Inclusive
                Generator.from<'t> |> Gen.map Exclusive
                Gen.constant Unbounded
            ]
        let rec genRange =
            gen {
                let! lower = genBound
                let! upper = genBound
                match Range.tryCreate lower upper with
                    | Some range -> return range
                    | None -> return! genRange
            }
        genRange |> Arb.fromGen

type Triplet<'t when 't : comparison and 't :> IComparable<'t>> =
    {
        AOpt : Option<'t>
        B : 't
        COpt : Option<'t>
    }

    static member Arb =
        let valueGen = Generator.from<'t>
        gen {
            let! aOpt =
                valueGen |> Gen.optionOf
            let! b =
                valueGen
                    |> Gen.where (fun b ->
                        aOpt
                            |> Option.map (fun a -> b > a)
                            |> Option.defaultValue true)
            let! cOpt =
                valueGen
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

module ``Range tests`` =

    let private compare lower upper =
        match lower, upper with
            | Inclusive lower, Inclusive upper ->
                lower <= upper
            | Inclusive lower, Exclusive upper
            | Exclusive lower, Inclusive upper
            | Exclusive lower, Exclusive upper ->
                lower < upper
            | _ -> true                

    [<Property>]
    let ``Lower value < or <= upper value`` (range : Range<int>) =
        compare range.Lower range.Upper

    [<Property>]
    let ``DateTime type`` (range : Range<System.DateTime>) =
        compare range.Lower range.Upper

    [<Property>]
    let ``String type`` (range : Range<string>) =
        compare range.Lower range.Upper

    [<Property>]
    let ``Bounds are in or out of range`` (range : Range<int>) =
        let test = function
            | Inclusive x -> range |> Range.contains x
            | Exclusive x -> range |> Range.contains x |> not
            | Unbounded -> true
        test range.Lower && test range.Upper

    [<Property>]
    let ``Bounded operators`` () =
        let rangeA = 1 +-+ 3
        let rangeB = 2 *-* 4
        Range.union [rangeA] [rangeB] = [1 +-* 4]
            && Range.intersection [rangeA] [rangeB] = [2 *-+ 3]
            && Range.difference [rangeA] [rangeB] = [1 +-+ 2]
            && Range.difference [rangeB] [rangeA] = [3 *-* 4]

    [<Property>]
    let ``Unbounded operators`` () =
        let rangeA = !-+ 1
        let rangeB = !*- -1
        Range.union [rangeA] [rangeB] = [Range.infinite]
            && Range.intersection [rangeA] [rangeB] = [-1 *-+ 1]
            && Range.difference [rangeA] [rangeB] = [!-+ -1]
            && Range.difference [rangeB] [rangeA] = [!*- 1]

    [<Property>]
    let ``Psuedo-empty`` () =
        let rangeA = 0 +-+ 1
        let rangeB = 2 +-+ 3
        let weird =  1 *-* 2
        let full = Range.union [ rangeA; rangeB ] [ weird ]
        let partial = Range.union [ rangeA ] [ rangeB ]
        full.Length = 1
            && partial.Length = 2
            && full <> partial

module ``Merge tests`` =

    [<Property>]
    let ``Merged ranges are a superset of all ranges`` (ranges : List<Range<int>>) =
        let merged = Range.merge ranges
        ranges
            |> Seq.forall (fun range ->
                Range.merge (range :: merged) = merged)

    [<Property>]
    let ``Merge of no ranges is empty`` () =
        Range.merge List.empty<Range<int>> = List.empty<Range<int>>

    [<Property>]
    let ``Merge of range by itself is self`` (range : Range<int>) =
        Range.merge [range] = [range]

module ``Union tests`` =

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
    let ``Union merges adjancent ranges`` (triplet : Triplet<int>) =
        let boundA =
            triplet.AOpt
                |> Option.map Inclusive
                |> Option.defaultValue Unbounded
        let boundC =
            triplet.COpt
                |> Option.map Inclusive
                |> Option.defaultValue Unbounded
        let rangeABIncl = Range.create boundA (Inclusive triplet.B)
        let rangeABExcl = Range.create boundA (Exclusive triplet.B)
        let rangeBCIncl = Range.create (Inclusive triplet.B) boundC
        let rangeBCExcl = Range.create (Exclusive triplet.B) boundC
        let rangeAC = Range.create boundA boundC
        Range.union [rangeABIncl] [rangeBCIncl] = [rangeAC]
            && Range.union [rangeABIncl] [rangeBCExcl] = [rangeAC]
            && Range.union [rangeABExcl] [rangeBCIncl] = [rangeAC]
            && Range.union [rangeABExcl] [rangeBCExcl] = [rangeABExcl; rangeBCExcl]

module ``Intersection tests`` =

    [<Property>]
    let ``Intersection of ranges is a subset of all ranges`` (ranges : List<Range<int>>) =
        let intersection =
            ranges
                |> Seq.map List.singleton
                |> Seq.fold Range.intersection [Range.infinite]
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
        let ranges = Range.merge ranges
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
    let ``Intersection of adjancent ranges is a point at most`` (triplet : Triplet<int>) =
        let boundA =
            triplet.AOpt
                |> Option.map Inclusive
                |> Option.defaultValue Unbounded
        let boundC =
            triplet.COpt
                |> Option.map Inclusive
                |> Option.defaultValue Unbounded
        let rangeABIncl = Range.create boundA (Inclusive triplet.B)
        let rangeABExcl = Range.create boundA (Exclusive triplet.B)
        let rangeBCIncl = Range.create (Inclusive triplet.B) boundC
        let rangeBCExcl = Range.create (Exclusive triplet.B) boundC
        let rangeB = Range.singleton triplet.B
        Range.intersection [rangeABIncl] [rangeBCIncl] = [rangeB]
            && Range.intersection [rangeABIncl] [rangeBCExcl] = []
            && Range.intersection [rangeABExcl] [rangeBCIncl] = []
            && Range.intersection [rangeABExcl] [rangeBCExcl] = []

module ``Inverse tests`` =

    let (.||.) = Range.union
    let (.&&.) = Range.intersection
    let (~~) = Range.inverse

    [<Property>]
    let ``Double inverse is identity`` (ranges : List<Range<int>>) =
        let ranges = Range.merge ranges
        ~~(~~ranges) = ranges

    [<Property>]
    let ``De Morgan's laws``
        (A : List<Range<int>>)
        (B : List<Range<int>>) =
        let first = ~~(A .||. B) = (~~A .&&. ~~B)
        let second = ~~(A .&&. B) = (~~A .||. ~~B)
        first && second
