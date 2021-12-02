namespace FRange

open FsCheck

module Program =

    [<EntryPoint>] 
    let main args =

        let config =
            {
                Config.Default with
                    Arbitrary = [ typeof<Generators> ]
                    MaxTest = 10000
            }
        Check.One(config, ``Inverse tests``.``De Morgan's laws``)

        0
