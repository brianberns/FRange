namespace FRange

type Bound<'t when 't : comparison> =
    | Inclusive of 't
    | Exclusive of 't

type Range<'t when 't : comparison> =
    | SpanRange of lower : Bound<'t> * upper : Bound<'t>
    | SingletonRange of 't
    | LowerBoundRange of lower : Bound<'t>
    | UpperBoundRange of upper : Bound<'t>
    | InfiniteRange

module Range =

    let lbi x =
        x |> Inclusive |> LowerBoundRange

    let lbe x =
        x |> Exclusive |> LowerBoundRange

    let ubi x =
        x |> Inclusive |> UpperBoundRange

    let ube x =
        x |> Exclusive |> UpperBoundRange

    let inf = InfiniteRange

    let inRange x = function
        | SpanRange (Inclusive lower, Inclusive upper) ->
            x >= lower && x <= upper
        | SpanRange (Exclusive lower, Inclusive upper) ->
            x > lower && x <= upper
        | SpanRange (Inclusive lower, Exclusive upper) ->
            x >= lower && x < upper
        | SpanRange (Exclusive lower, Exclusive upper) ->
            x > lower && x < upper
        | SingletonRange value ->
            x = value
        | LowerBoundRange (Inclusive lower) ->
            x >= lower
        | LowerBoundRange (Exclusive lower) ->
            x > lower
        | UpperBoundRange (Inclusive upper) ->
            x <= upper
        | UpperBoundRange (Exclusive upper) ->
            x < upper
        | InfiniteRange -> true

    let inRanges x ranges =
        ranges |> Seq.exists (inRange x)

    let aboveRange x = function
        | SpanRange (_, Inclusive upper)
        | UpperBoundRange (Inclusive upper) ->
            x > upper
        | SpanRange (_, Exclusive upper)
        | UpperBoundRange (Exclusive upper) ->
            x >= upper
        | SingletonRange value ->
            x > value
        | LowerBoundRange _
        | InfiniteRange -> false

    let aboveRanges x ranges =
        ranges |> Seq.forall (aboveRange x)

    let belowRange x = function
        | SpanRange (Inclusive lower, _)
        | LowerBoundRange (Inclusive lower) ->
            x < lower
        | SpanRange (Exclusive lower, _)
        | LowerBoundRange (Exclusive lower) ->
            x <= lower
        | SingletonRange value ->
            x < value
        | UpperBoundRange _
        | InfiniteRange -> false

    let belowRanges x ranges =
        ranges |> Seq.forall (belowRange x)

[<AutoOpen>]
module RangeOperators =

    let (+=+) lower upper =
        SpanRange (Inclusive lower, Inclusive upper)

    let ( *=+) lower upper =
        SpanRange (Exclusive lower, Inclusive upper)

    let (+=*) lower upper =
        SpanRange (Inclusive lower, Exclusive upper)

    let ( *=*) lower upper =
        SpanRange (Exclusive lower, Exclusive upper)
