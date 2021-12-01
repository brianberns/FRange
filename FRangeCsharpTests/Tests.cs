using Microsoft.VisualStudio.TestTools.UnitTesting;
using FRange.CSharp;

[TestClass]
public class Tests
{
    [TestMethod]
    public void TestMethod1()
    {
        var rangeA = Range.Create(1, BoundType.Inclusive, 3, BoundType.Inclusive);
        var rangeB = Range.Create(2, BoundType.Exclusive, 4, BoundType.Exclusive);
        System.Console.WriteLine(rangeA);
    }
}
