namespace FastText
[<AutoOpen>]
module Heap =
    open System.Collections.Generic
    type MinHeap(array : ResizeArray<KeyValuePair<float,int>>) = 
        new(cap : int) = MinHeap(new ResizeArray<KeyValuePair<float,int>>(cap))
        new() = MinHeap(4)
        member x.Add(k : float, v : int) =
            array.Add(KeyValuePair(k,v))
            let mutable c = array.Count - 1;
            let mutable  parent = (c - 1) >>> 1
            while c > 0 && array.[c].Key.CompareTo(array.[parent].Key) < 0 do
                let tmp = array.[c];
                array.[c] <- array.[parent]
                array.[parent] <- tmp
                c <- parent
                parent <- (c - 1) >>> 1

        member x.RemoveMin() = 
            let ret = array.[0]
            array.[0] <- array.[array.Count - 1]
            array.RemoveAt(array.Count - 1)

            let rec cycle(c) = 
                if c < array.Count
                then    let mutable min = c
                        if 2 * c + 1 < array.Count && array.[2 * c + 1].Key.CompareTo(array.[min]) = -1
                        then min <- 2 * c + 1
                        if 2 * c + 2 < array.Count && array.[2 * c + 2].Key.CompareTo(array.[min]) = -1
                        then min <- 2 * c + 2
                        if min = c then ret
                        else let tmp = array.[c]
                             array.[c] <- array.[min]
                             array.[min] <- tmp
                             cycle(min)
                else ret
            cycle(0)

        member x.Peek() = array.[0]

        member x.Count = array.Count

        member this.Item with get(index) = array.[index]

//class PriorityQueue<T>
//{
//    internal class Node : IComparable<Node>
//    {
//        public int Priority;
//        public T O;
//        public int CompareTo(Node other)
//        {
//            return Priority.CompareTo(other.Priority);
//        }
//    }
//
//    private MinHeap<Node> minHeap = new MinHeap<Node>();
//
//    public void Add(int priority, T element)
//    {
//        minHeap.Add(new Node() { Priority = priority, O = element });
//    }
//
//    public T RemoveMin()
//    {
//        return minHeap.RemoveMin().O;
//    }
//
//    public T Peek()
//    {
//        return minHeap.Peek().O;
//    }
//
//    public int Count
//    {
//        get
//        {
//            return minHeap.Count;
//        }
//    }
//}

