namespace FRange

open System

open FsCheck
open FsCheck.Xunit

open Range

type UnequalPair<'t> =
    MkUnequalPair of 't * 't

type Generators =

    static member private UnequalPair(gen) =
        {
            new Arbitrary<_>() with
                override _.Generator =
                    gen
                        |> Gen.two
                        |> Gen.filter (fun (x, y) -> x <> y)
                        |> Gen.map (fun (x, y) -> MkUnequalPair (x, y))
                override _.Shrinker(_) = Seq.empty
        }

    static member UnequalPairInt() =
        Arb.Default.Int32().Generator
            |> Generators.UnequalPair

    static member UnequalPairDateTime() =
        Arb.Default.DateTime().Generator
            |> Generators.UnequalPair

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
        not <| inRange second (SingletonRange first)

    [<Property(Arbitrary = [| typeof<Generators> |])>]
    let ``Singleton int not in range`` (pair : UnequalPair<int>) =
        ``Singleton not in range`` pair

    [<Property(Arbitrary = [| typeof<Generators> |])>]
    let ``Singleton DateTime not in range`` (pair : UnequalPair<DateTime>) =
        ``Singleton not in range`` pair
