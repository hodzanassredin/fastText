namespace FastText
module Matrix =
    open System.Runtime.CompilerServices
    type Vector = 
        struct
          val offset : int
          val m : int
          val v : float32[]

          new(m :int) = {offset = 0; m = m; v = Array.zeroCreate m} 
          new(m :int, offset : int, v : float32[]) = {offset = offset; m = m; v = v} 
          member x.set(i:int,v:float32) = x.v.[i + x.offset] <- v
          member x.Item
              with get i = x.v.[i + x.offset]
              and set i v = x.v.[i + x.offset] <- v
        end

    type Matrix =
       struct
          val m: int
          val n: int
          val private v : float32[]
          new(m :int, n :int) = {m = m; n = n; v = Array.zeroCreate (m * n)} 
          member this.set(i,j,value) = this.v.[i * this.n + j] <- value
          member this.Item
              with get(i,j) = this.v.[i * this.n + j]
              and set (i,j) value = this.v.[i * this.n + j] <- value
          member this.Vector(i) = Vector(this.n, i * this.n, this.v)
       end

    let createVector(m) : Vector = Vector(m)
    let create(m,n) = Matrix(m, n)
    let createNull() = Matrix(0,0)

    let load(inp : System.IO.BinaryReader) =
          let m = int(inp.ReadInt64())
          let n = int(inp.ReadInt64())
          let mutable r = create(m, n)
          for i = 0 to m - 1 do
            for j = 0 to n - 1 do 
                r.[i,j] <- inp.ReadSingle()
          r

    let save(this: Matrix, out : System.IO.BinaryWriter) =
          out.Write(int64(this.m))//todo int int64
          out.Write(int64(this.n))
          for i = 0 to this.m - 1 do
            for j = 0 to this.n - 1 do 
                out.Write(this.[i,j])

    let simdAdd (v1 : Vector) (v2 : Vector) =
        let buffer_size = System.Numerics.Vector<float32>.Count
        let q = v1.m / buffer_size
        let n = q * buffer_size
        let mutable i = 0
        while i < v1.m do
            let va = new System.Numerics.Vector<float32>(v1.v, i + v1.offset)
            let vb = new System.Numerics.Vector<float32>(v2.v, i + v2.offset)
            let vc = va + vb
            vc.CopyTo(v1.v, i + v1.offset)
            i <- i + buffer_size

    [<Extension>]
    type VectorExts() =
        [<Extension>]
        static member inline Zero(this: Vector) = 
            for i = 0 to this.m - 1 do
                this.set(i, 0.0f)
        [<Extension>]
        static member inline Mul(this: Vector, a : float32) = 
            for i = 0 to this.m - 1 do
                this.set(i, this.[i] * a)
        [<Extension>]
        static member inline AddRow(this: Vector, A : Matrix, i : int) =  
            assert (i >= 0)
            assert (i < A.m)
            assert (this.m = A.n)
            for j in 0..(A.n - 1) do
                this.set(j, this.[j] + A.[i, j])
        [<Extension>]
        static member inline AddRow(this: Vector, A : Matrix, i : int, a : float32) =  
            assert (i >= 0)
            assert (i < A.m)
            assert (this.m = A.n)
            for j in 0..(A.n - 1) do
                this.set(j, this.[j] + a * A.[i, j])
        [<Extension>]
        static member inline Mul(this: Vector, A : Matrix, vec : Vector) =
          assert(A.m = this.m)
          assert(A.n = vec.m)
          for i in 0..this.m - 1 do
            this.set(i, 0.0f)
            for j in 0..(A.n - 1) do
              this.set(i, this.[i] + A.[i, j] * vec.[j])
        [<Extension>]
        static member inline Argmax(this: Vector) =
          let mutable max = this.[0]
          let mutable argmax = 0
          for i in 0..this.m - 1 do
            if this.[i] > max then
              max <- this.[i]
              argmax <- i
          argmax
        [<Extension>]
        static member inline WriteTo(this: Vector, s:System.IO.BinaryWriter) = 
            for i = 0 to this.m - 1 do
                s.Write(this.[i])
                s.Write(Utils.spaceCode)

    [<Extension>]
    type MatrixExts() =
        [<Extension>]
        static member inline Zero(this: Matrix) = 
            for i = 0 to this.m - 1 do
               for j = 0 to this.n - 1 do
                    this.set(i, j, 0.0f)

//        member x.Set(other : Matrix) = 
//            x.M <- other.M
//            x.N <- other.N
//            x.Data <- Array.copy other.Data
        [<Extension>]
        static member inline Uniform(this: Matrix, a : float32) = 
            let rng = Random.Mcg31m1(1)
            for i = 0 to this.m - 1 do
               for j = 0 to this.n - 1 do
                    this.set(i,j, rng.ConUniformSample(-a,a))
        [<Extension>]
        static member inline AddRow(this: Matrix, vec : Vector, i, a) =
          assert(i >= 0)
          assert(i < this.m)
          assert(vec.m = this.n)

          for j = 0 to vec.m - 1 do
            this.set(i, j, this.[i, j] + a * vec.[j])

        [<Extension>]
        static member inline DotRow(this: Matrix, vec : Vector, i) =
          assert(i >= 0)
          assert(i < this.m)
          assert(vec.m = this.n)
          let mutable d = 0.0f
          for j = 0 to vec.m - 1 do
            d <- d + this.[i, j] * vec.[j]
          d