namespace FRange

open System

open FsCheck
open FsCheck.Xunit

open Range

type UnequalPair<'t> =
    MkUnequalPair of 't * 't

type SpanContains =
    MkSpanContains of (int * int) * int

type Generators =

    static member private UnequalPair(g) =
        gen {
            let! x = g
            let! y = g |> Gen.where ((<>) x)
            return MkUnequalPair (x, y)
        } |> Arb.fromGen

    static member UnequalPairInt() =
        Arb.Default.Int32().Generator
            |> Generators.UnequalPair

    static member UnequalPairDateTime() =
        Arb.Default.DateTime().Generator
            |> Generators.UnequalPair

    static member SpanContains() =
        let g = Arb.Default.Int32().Generator
        gen {
            let! lower = g
            let! upper = g |> Gen.where (fun x -> x >= lower)
            let! middle = Gen.choose (lower, upper)
            return MkSpanContains ((lower, upper), middle)
        } |> Arb.fromGen

module Tests =

    let private ``Singleton in range`` x =
        inRange x (SingletonRange x)

    [<Property>]
    let ``Singleton int in range`` (x : int) =
        ``Singleton in range`` x

    [<Property>]
    let ``Singleton DateTime in range`` (x : DateTime) =
        ``Singleton in range`` x

    let private ``Singleton not in range`` (MkUnequalPair (first, second)) =
        first
            |> SingletonRange
            |> inRange second
            |> not

    [<Property>]
    let ``Singleton int not in range`` (pair : UnequalPair<int>) =
        ``Singleton not in range`` pair

    [<Property>]
    let ``Singleton DateTime not in range`` (pair : UnequalPair<DateTime>) =
        ``Singleton not in range`` pair

    [<Property>]
    let ``Span contains`` (MkSpanContains ((lower, upper), middle)) =
        SpanRange (Inclusive lower, Inclusive upper)
            |> inRange middle

    [<assembly: Properties(Arbitrary = [| typeof<Generators> |])>]
    do ()
