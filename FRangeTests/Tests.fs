namespace FRange

open FsCheck.Xunit

module Tests =

    [<Property>]
    let ``Singleton in range`` (x : int) =
        Range.inRange x (SingletonRange x)
