using Microsoft.VisualStudio.TestTools.UnitTesting;
using FRange.CSharp;

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
        var mrA = Range.Create(1, BoundType.Inclusive, 3, BoundType.Inclusive).ToMultiRange();
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
    }
}
