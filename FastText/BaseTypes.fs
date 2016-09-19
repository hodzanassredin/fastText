namespace FastText
[<AutoOpen>]
module BaseTypes = 
    open System.Runtime.CompilerServices

    [<Extension>]
    type ResizeArrExts() =
        [<Extension>]
        static member inline Resize(this: ResizeArray<'a>, size : int) = 
            if this.Count > size
            then this.RemoveRange(size, this.Count - size)
            else this.AddRange(System.Linq.Enumerable.Repeat(Unchecked.defaultof<'a>, size - this.Count))
        [<Extension>]
        static member inline ShrinkToFit(this: ResizeArray<'a>) = 
            if this.Count < this.Capacity
            then this.Capacity <- this.Count

    type String private (data : ResizeArray<byte>) =
         new() = String(ResizeArray<byte>())
         new(s : string) = String(ResizeArray<byte>(System.Text.Encoding.UTF8.GetBytes(s)))
         member x.Array = data
         member x.Clear() = data.RemoveRange(0, data.Count - 1)
         member x.Add(v) = data.Add(v)
         member x.AddRange(v) = data.AddRange(v)
         member x.Empty() = data.Count = 0
         member x.StartsWith(sub : String) = 
            let mutable i = 0
            if sub.Array.Count > x.Array.Count 
            then false
            else while x.Array.[i] = sub.Array.[i] && i < sub.Array.Count do
                    i <- i + 1
                 i = sub.Array.Count - 1
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

