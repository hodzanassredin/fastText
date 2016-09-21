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

        member x.ReadByte() = if pos = len 
                              then raise <| System.IO.EndOfStreamException()
                              let i = int(pos - buff_pos)
                              if i < buff_size then pos <- pos + 1L
                                                    buff.[i]
                              else s.Read(buff, 0, buff_size) |>ignore
                                   buff_pos <- pos
                                   pos <- pos + 1L
                                   buff.[0]

        member x.EOF() = pos >= len
        member x.NotEOF() = pos < len
        member x.MoveAbs(l) = s.Position <- l
                              pos <- l
                              buff_pos <- l
                              s.Read(buff, 0, buff_size) |>ignore

        member x.Unget() = assert (pos - buff_pos > 0L) 
                           pos <- pos - 1L
                           
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

    [<NoEquality>]
    [<NoComparison>]
    type String private (data : ResizeArray<byte>) =
         new() = String(ResizeArray<byte>())
         new(s : string) = String(ResizeArray<byte>(System.Text.Encoding.UTF8.GetBytes(s)))
         member x.Array = data
         member x.Clear() = data.RemoveRange(0, data.Count)
         member x.Add(v) = data.Add(v)
         member x.AddRange(v) = data.AddRange(v)
         member x.Empty() = data.Count = 0
         member x.Copy() = String(ResizeArray<byte>(data.ToArray()))
         member x.StartsWith(sub : String) = 
            let mutable i = 0
            if sub.Array.Count > x.Array.Count 
            then false
            else while i < sub.Array.Count && x.Array.[i] = sub.Array.[i] do
                    i <- i + 1
                 i = sub.Array.Count
         member x.Count = data.Count
         member this.Item
              with get(index) = data.[index]
              and set index value = data.[index] <- value

         override x.ToString() = System.Text.Encoding.UTF8.GetString(data.ToArray())
         static member (+) (v : String, a : String) =
            let sum = ResizeArray<byte>(v.Array.Count + a.Array.Count)
            sum.AddRange(v.Array)
            sum.AddRange(a.Array)
            String(sum)

         static member inline (==) (x : String, y : String) =
            x.Array.Count = y.Array.Count && x.StartsWith(y)

         member str.hash() : uint32 =
          let mutable h = 2166136261u
          for i = 0 to str.Count - 1 do
            h <- h ^^^ uint32(str.[i])
            h <- h * 16777619u
          h

         override x.GetHashCode() = hash x.Array

         interface System.IComparable with
          member x.CompareTo yobj =
              match yobj with
              | :? String as y -> failwith "not implemented"
              | _ -> invalidArg "yobj" "cannot compare values of different types"


