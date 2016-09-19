namespace FastText
module Matrix =
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions
    type Vector(m)=
        let data_:float[] = Array.create m 0.0
        member x.M = m
        member x.Data = data_
        member x.Zero() = Array.fill data_ 0 (m - 1) 0.0
        member x.Mul(a : float) = Array.iteri (fun i v -> data_.[i] <- v * a) data_
        member x.AddRow(A : Matrix, i : int) =  
            assert (i >= 0)
            assert (i < A.M)
            assert (m = A.N)
            for j in 0..(A.N - 1) do
                data_.[j] <- data_.[j] + A.[i, j]

        member x.AddRow(A : Matrix, i : int, a : float) =  
            assert (i >= 0)
            assert (i < A.M)
            assert (m = A.N)
            for j in 0..(A.N - 1) do
                data_.[j] <- data_.[j] + a * A.[i, j]

        member x.Mul(A : Matrix, vec : Vector) =
          assert(A.M = m)
          assert(A.N = vec.M)
          for i in 0..m - 1 do
            data_.[i] <- 0.0
            for j in 0..A.N - 1 do
              data_.[i] <- data_.[i] + A.[i, j] * vec.Data.[j]

        member x.Argmax() =
          let mutable max = data_.[0]
          let mutable argmax = 0
          for i in 0..m - 1 do
            if data_.[i] > max then
              max <- data_.[i]
              argmax <- i
          argmax

        member x.WriteTo(s:System.IO.BinaryWriter) = 
            Array.iter (fun (v:float) -> s.Write(v);s.Write(Utils.spaceCode)) data_

    and Matrix private (mi,ni,data_i : float[])=
        let mutable m = mi
        let mutable n = ni
        let mutable data_ = data_i
        new(m,n) = Matrix(m,n, Array.create(m * n) 0.0)
        new() = Matrix(0,0)
        new(other : Matrix) = Matrix(other.M, other.N, Array.copy other.Data)
            
        member this.M
          with get() = m
          and set(value) = m <- value
        member this.N
          with get() = n
          and set(value) = n <- value
        member this.Data
          with get() = data_
          and set(value) = data_ <- value
        member this.Item
          with get(i,j) = data_.[i*n + j]
          and set (i,j) value = data_.[i*n + j] <- value
        member x.Zero() = Array.fill data_ 0 (data_.Length - 1) 0.
        member x.Set(other : Matrix) = 
            x.M <- other.M
            x.N <- other.N
            x.Data <- Array.copy other.Data

        member x.Uniform(a : float) = //Random.doubleFill data_
            let rng = Mcg31m1(1)
            let uniform = ContinuousUniform(-a,a, rng)
            for i in 0..(m * n - 1) do
                data_.[i] <- float(uniform.Sample())

        member x.AddRow(vec : Vector, i, a) =
          assert(i >= 0)
          assert(i < m)
          assert(vec.M = n)
          for j = 0 to n - 1 do
            data_.[i * n + j] <- data_.[i * n + j] + a * vec.Data.[j]

        member x.DotRow(vec : Vector, i) =
          assert(i >= 0)
          assert(i < m)
          assert(vec.M = n)
          let mutable d = 0.0
          for j = 0 to n - 1 do
            d <- d + data_.[i * n + j] * vec.Data.[j]
          d

        member x.save(out : System.IO.BinaryWriter) =
          out.Write(int64(m))//todo int int64
          out.Write(int64(n))
          Array.iter (fun (v:float) -> out.Write(v)) data_

        member x.load(inp : System.IO.BinaryReader) =
          m <- int(inp.ReadInt64())
          n <- int(inp.ReadInt64())
          data_ <- Array.zeroCreate (m * n)
          for i = 0 to m * n do
               data_.[i] <- inp.ReadDouble()
