# FRange

FRange is a .NET library for working with ranges of values. If you have a sortable type (e.g. `int`, `DateTime`, `string`, etc.), you can create ranges of its values, and then compute unions, intersections, etc. of those ranges.

Ranges can be bounded or unbounded on both sides, and bounds can be either inclusive or exclusive. For example `x < 3` has an exclusive upper bound and no lower bound, while `0.0 <= x <= 1.0` is bounded inclusively on both sides.

FRange has separate API's for F# and C#.

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

The `+-*` operator creates a range that has an inclusive lower bound (symbolized by `+`) and an exclusive upper bound (symbolized by `*`).

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

## Usage (C#)

We can translate the same example to C# using the `FRange.CSharp` namespace:

```csharp
using FRange.CSharp;
using Range = FRange.CSharp.Range;   // resolve conflict with System.Range
```

Use `MultiRange` to hold collections of ranges, like this:

```csharp
var aSchedule =
    MultiRange.Create(
        new[] {
            Range.Create(
                DateTime.Parse("11:30am"), BoundType.Inclusive,
                DateTime.Parse("1pm"), BoundType.Exclusive),
            Range.Create(
                DateTime.Parse("2:30pm"), BoundType.Inclusive,
                DateTime.Parse("4:30pm"), BoundType.Exclusive)
        });
var bSchedule =
    MultiRange.Create(
        new[] {
            Range.Create(
                DateTime.Parse("10am"), BoundType.Inclusive,
                DateTime.Parse("11am"), BoundType.Exclusive),
            Range.Create(
                DateTime.Parse("11am"), BoundType.Inclusive,
                DateTime.Parse("3pm"), BoundType.Exclusive)
        });
```

Then determine mutually free time like this:

```csharp
var day =
    Range.Create(
        DateTime.Parse("9am"), BoundType.Inclusive,
        DateTime.Parse("5pm"), BoundType.Exclusive)
        .ToMultiRange();
var free =
    day.Difference(aSchedule.Union(bSchedule));
```

Note that we've used `ToMultiRange` in this case to create a multi-range that contains a single range. The result is the same as above:

```
[12/1/2021 9:00:00 AM <= x < 12/1/2021 10:00:00 AM;
 12/1/2021 4:30:00 PM <= x < 12/1/2021 5:00:00 PM]
```