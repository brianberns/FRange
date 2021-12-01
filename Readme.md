# FRange

FRange is a .NET library for working with ranges of values. If you have a sortable type (e.g. `int`, `DateTime`, `string`, etc.), then you can create ranges of its values. Then you can compute unions, intersections, etc. of those ranges. FRange has API's for both F# and C#.

Ranges can be bounded or unbounded on both sides, and bounds can be either inclusive or exclusive. For example `x < 3` has an exclusive upper bound and no lower bound, while `0.0 <= x <= 1.0` is bounded inclusively on both sides.

## Usage (F#)

Let's say Alice has the following schedule:

```fsharp
let parse = DateTime.Parse
let aSchedule =
    [|
        parse "11:30am" +-* parse "1pm"
        parse "2:30pm" +-* parse "4:30pm"
    |]
```

The `+-*` operator creates a range that has an inclusive lower bound (symbolized by `+`) and and exclusive upper bound (symbolized by `*`). 
Using this same pattern, we can also create a schedule for Bob:

```fsharp
let bSchedule =
    [|
        parse "10am" +-* parse "11am"
        parse "11am" +-* parse "3pm"
    |]
```

If the workday goes from 9am to 5pm, what times during the day are both Alice and Bob free? We can determine this as follows:

```fsharp
let day = parse "9am" +-* parse "5pm"
let free =
    Range.difference [day]
        (Range.union aSchedule bSchedule)
```

`Range.union` determines the union of two schedules, and then `Range.difference` determines what times during the day are not occupied by either schedule. The result is:

```
[12/1/2021 9:00:00 AM <= x < 12/1/2021 10:00:00 AM;
 12/1/2021 4:30:00 PM <= x < 12/1/2021 5:00:00 PM]
```

So the mutually free times are 9-10am and 4:30-5pm.