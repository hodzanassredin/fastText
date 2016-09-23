namespace FastText
module Matrix =
    open System.Runtime.CompilerServices
    type Vector = float[]
    type Matrix  = float[,]
    let createVector(m) : Vector = Array.zeroCreate m

    let create(m,n) = Array2D.zeroCreate m n
    let createNull() = create(0,0)
    let copy(other : Matrix) : Matrix = Array2D.copy other

    let load(inp : System.IO.BinaryReader) =
          let m = int(inp.ReadInt64())
          let n = int(inp.ReadInt64())
          let r = Array2D.zeroCreate m n
          for i = 0 to m - 1 do
            for j = 0 to n - 1 do 
                r.[i,j] <- inp.ReadDouble()
          r

    let save(this: Matrix, out : System.IO.BinaryWriter) =
          out.Write(int64(Array2D.length1 this))//todo int int64
          out.Write(int64(Array2D.length2 this))
          Array2D.iter (fun (v:float) -> out.Write(v)) this

    let inline m(this : Matrix) = Array2D.length1 this
    let inline n(this : Matrix) = Array2D.length2 this

    [<Extension>]
    type VectorExts() =
        [<Extension>]
        static member inline Zero(this: Vector) = 
            Array.fill this 0 (this.Length - 1) 0.0
        [<Extension>]
        static member inline M(this: Vector) = this.Length
        [<Extension>]
        static member inline Mul(this: Vector, a : float) = 
            for i = 0 to this.Length - 1 do
                this.[i] <- this.[i] * a
        [<Extension>]
        static member inline AddRow(this: Vector, A : Matrix, i : int) =  
            assert (i >= 0)
            assert (i < m A)
            assert (this.Length = n A)
            for j in 0..(n A - 1) do
                this.[j] <- this.[j] + A.[i, j]
        [<Extension>]
        static member inline AddRow(this: Vector, A : Matrix, i : int, a : float) =  
            assert (i >= 0)
            assert (i < m A)
            assert (this.Length = n A)
            for j in 0..(n A - 1) do
                this.[j] <- this.[j] + a * A.[i, j]
        [<Extension>]
        static member inline Mul(this: Vector, A : Matrix, vec : Vector) =
          assert(m A = this.Length)
          assert(n A = vec.Length)
          for i in 0..this.Length - 1 do
            this.[i] <- 0.0
            for j in 0..(n A - 1) do
              this.[i] <- this.[i] + A.[i, j] * vec.[j]
        [<Extension>]
        static member inline Argmax(this: Vector) =
          let mutable max = this.[0]
          let mutable argmax = 0
          for i in 0..this.Length - 1 do
            if this.[i] > max then
              max <- this.[i]
              argmax <- i
          argmax
        [<Extension>]
        static member inline WriteTo(this: Vector, s:System.IO.BinaryWriter) = 
            Array.iter (fun (v:float) -> s.Write(v);s.Write(Utils.spaceCode)) this

    [<Extension>]
    type MatrixExts() =
        [<Extension>]
        static member inline M(this: Matrix) = Array2D.length1 this
        [<Extension>]
        static member inline N(this: Matrix) = Array2D.length2 this
        [<Extension>]
        static member inline Zero(this: Matrix) = 
            for i = 0 to m this - 1 do
               for j = 0 to n this - 1 do
                    this.[i,j] <- 0.0

//        member x.Set(other : Matrix) = 
//            x.M <- other.M
//            x.N <- other.N
//            x.Data <- Array.copy other.Data
        [<Extension>]
        static member inline Uniform(this: Matrix, a : float) = 
            let rng = Random.Mcg31m1(1)
            for i = 0 to m this - 1 do
               for j = 0 to n this - 1 do
                    this.[i,j] <- rng.ConUniformSample(-a,a)
        [<Extension>]
        static member inline AddRow(this: Matrix, vec : Vector, i, a) =
          assert(i >= 0)
          assert(i < m this)
          assert(vec.Length = n this)

          for j = 0 to vec.Length - 1 do
            this.[i, j] <- this.[i, j] + a * vec.[j]

        [<Extension>]
        static member inline DotRow(this: Matrix, vec : Vector, i) =
          assert(i >= 0)
          assert(i < m this)
          assert(vec.Length = n this)
          let mutable d = 0.0
          for j = 0 to vec.Length - 1 do
            d <- d + this.[i, j] * vec.[j]
          d