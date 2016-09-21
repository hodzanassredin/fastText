namespace FastText
[<AutoOpen>]
module BaseTypes = 
    open System.Runtime.CompilerServices
    open System.IO
    [<Extension>]
    type Exts() =
        [<Extension>]
        static member inline Resize(this: ResizeArray<'a>, size : int) = 
            if this.Count > size
            then this.RemoveRange(size, this.Count - size)
            else this.AddRange(System.Linq.Enumerable.Repeat(Unchecked.defaultof<'a>, size - this.Count))
        [<Extension>]
        static member inline ShrinkToFit(this: ResizeArray<'a>) = 
            if this.Count < this.Capacity
            then this.Capacity <- this.Count

    type BinaryReader (s : System.IO.Stream) = 
        let buff_size = 10000000
        let buff : byte[] = Array.zeroCreate buff_size 
        let len = s.Length
        let mutable pos = s.Position
        let mutable buff_pos = pos
        do
            s.Read(buff, 0, buff_size) |>ignore
        new(filename) = let stream = System.IO.File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
                        new BinaryReader(stream)

        member x.ReadByte() = if pos >= len 
                              then raise <| System.IO.EndOfStreamException()
                              let i = int(pos - buff_pos)
                              if i < 0 //unget handle
                              then pos <- 0L
                                   0x0auy // \n
                              elif i < buff_size 
                              then pos <- pos + 1L
                                   buff.[i]
                              else s.Read(buff, 0, buff_size) |>ignore
                                   buff_pos <- pos
                                   pos <- pos + 1L
                                   buff.[0]

        member x.EOF() = pos >= len
        member x.NotEOF() = pos < len
        member x.MoveAbs(l) = if l >= buff_pos && l < (buff_pos + int64(buff_size))
                              then pos <- l
                              else s.Position <- l
                                   pos <- l
                                   buff_pos <- l
                                   s.Read(buff, 0, buff_size) |>ignore

        member x.Unget() = pos <- pos - 1L
                           
        member x.Length = len
        member x.Close() = s.Close()
        member x.Reader() = new System.IO.BinaryReader(s)

        interface System.IDisposable with 
            member this.Dispose() = s.Dispose()

    type BinaryWriter(w : System.IO.BinaryWriter) =
        new(stream) = new BinaryWriter(new System.IO.BinaryWriter(stream))
        new(filename) = new BinaryWriter(new System.IO.BinaryWriter(System.IO.File.Open(filename, System.IO.FileMode.Create)))
        member x.Close() = w.Close()

        member x.Writer() = w

        interface System.IDisposable with 
            member this.Dispose() = w.Dispose()
[<AutoOpen>]
module ByteString =

    type String = ResizeArray<byte>
    open System.Runtime.CompilerServices
    let fromString(s : string) = String(System.Text.Encoding.UTF8.GetBytes(s))
    
    [<Extension>]
    type ArrayExts () =
        [<Extension>]
        static member inline Clear(this : String) = this.RemoveRange(0, this.Count)
        [<Extension>]
        static member inline Copy(this : String) = ResizeArray<byte>(this.ToArray())
        [<Extension>]
        static member inline StartsWith(this : String, sub : String) = 
            let mutable i = 0
            if sub.Count > this.Count 
            then false
            else while i < sub.Count && this.[i] = sub.[i] do
                    i <- i + 1
                 i = sub.Count

        [<Extension>]
        static member inline ToStr(this : String) = 
            System.Text.Encoding.UTF8.GetString(this.ToArray())

        [<Extension>]
        static member inline Hash(this : String) = 
            let mutable h = 2166136261u
            for i = 0 to this.Count - 1 do
                h <- h ^^^ uint32(this.[i])
                h <- h * 16777619u
            h
        [<Extension>]
        static member inline Eq (x : String, y : String) =
            x.Count = y.Count && ArrayExts.StartsWith(x,y)

        [<Extension>]
        static member Wrap (v : String, pref : String, suff : String) =
            let sum = String(v.Count + pref.Count + suff.Count)
            sum.AddRange(pref)
            sum.AddRange(v)
            sum.AddRange(suff)
            sum






