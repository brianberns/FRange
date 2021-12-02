using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FRange.CSharp;
using Range = FRange.CSharp.Range;   // resolve conflict with System.Range

[TestClass]
public class Tests
{
    private void AssertEquivalent<T>(MultiRange<T> mrA, MultiRange<T> mrB)
    {
        Assert.IsTrue(mrA.IsEquivalent(mrB), mrB.ToString());
    }

    [TestMethod]
    public void MultiRanges()
    {
        var rangeA = Range.Create(1, BoundType.Inclusive, 3, BoundType.Inclusive);
        Assert.IsTrue(rangeA.HasLowerBound());
        Assert.AreEqual(3, rangeA.UpperBoundValue());
        Assert.AreEqual(BoundType.Inclusive, rangeA.UpperBoundType());

        var mrA = rangeA.ToMultiRange();
        var mrB = Range.Create(2, BoundType.Exclusive, 4, BoundType.Exclusive).ToMultiRange();

        var union = Range.Create(1, BoundType.Inclusive, 4, BoundType.Exclusive).ToMultiRange();
        AssertEquivalent(union, mrA.Union(mrB));

        var intersection = Range.Create(2, BoundType.Exclusive, 3, BoundType.Inclusive).ToMultiRange();
        AssertEquivalent(intersection, mrA.Intersection(mrB));

        var inverse =
            MultiRange.Create(new[]
                {
                    Range.CreateUpper(1, BoundType.Exclusive),
                    Range.CreateLower(4, BoundType.Inclusive)
                });
        AssertEquivalent(inverse, union.Inverse());
        Assert.IsTrue(inverse.Contains(-1));
    }

    static void Main()
    {
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
        var day =
            Range.Create(
                DateTime.Parse("9am"), BoundType.Inclusive,
                DateTime.Parse("5pm"), BoundType.Exclusive)
                .ToMultiRange();
        var free =
            day.Difference(aSchedule.Union(bSchedule));
        Console.WriteLine(free);
    }
}
