namespace FastText 
module Random =
    type Mcg31m1(seedInit) =
        let _modulus : uint64 = 2147483647UL
        let _multiplier : uint64 = 1132489760UL
        let _reciprocal : float = 1.0 / float(_modulus)
        let maxIntf = float(System.Int32.MaxValue)

        let seed = if seedInit = 0 then 1 else seedInit
        let mutable _xn : uint64 = uint64(seed) % _modulus

        new() = Mcg31m1(int(System.DateTime.Now.Ticks))

        member x.Next() = int(x.Sample() * maxIntf)

        member x.Sample() : float = 
            let ret = float(_xn) * _reciprocal
            _xn <- (_xn * _multiplier) % _modulus 
            ret

        member x.ConUniformSample(lower : float, upper : float) = 
            lower + (x.Sample() * (upper - lower))

        member x.DiscrUniformSample(lower : int, upper : int) =
            (x.Next() % (upper - lower + 1)) + lower

