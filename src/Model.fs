namespace FastText
module Model =
    open Matrix
    open Args
    open System.Linq
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions
    open System.Collections.Generic
    let NEGATIVE_TABLE_SIZE = 10000000
    type Node =
       struct
          val mutable parent: int
          val mutable left: int
          val mutable right: int
          val mutable count: int64
          val mutable binary : bool

          new(parent: int, left: int, right: int, count: int64, binary : bool) = 
            { 
                parent = parent
                left = left 
                right = right
                count = count
                binary = binary
            }
       end

    type Model(wi : Matrix, wo : Matrix, args : Args, seed) =
        let rng_ = Mcg31m1(1)
        let grad_ = Vector(args.Dim)
        let output_ = Vector(wo.M)
        let hidden_ = Vector(args.Dim)
        let codes : ResizeArray<ResizeArray<bool>> = ResizeArray<ResizeArray<bool>>()
        let paths : ResizeArray<ResizeArray<int>> = ResizeArray<ResizeArray<int>>()
        let isz_ = wi.M
        let osz_ = wo.M
        let hsz_ = args.Dim
        let mutable negpos = 0
        let mutable loss_ = 0.0
        let mutable nexamples_ = 1
        let tree  = ResizeArray<Node>()
        let negatives = ResizeArray<int>()

        let bfloat b = if b then 1. else 0.

        member x.rng = rng_
        member x.BinaryLogistic(target : int, label : bool, lr : float) =
            let score = Utils.sigmoid(wo.DotRow(hidden_, target))
            let alpha = lr * (bfloat(label) - score)
            grad_.AddRow(wo, target, alpha)
            wo.AddRow(hidden_, target, alpha)
            if label then -Utils.log(score)
            else -Utils.log(1.0 - score)

        member x.NegativeSampling(target : int, lr : float) =
          let mutable loss = 0.0
          grad_.Zero()
          for n = 0 to args.Neg do
            if n = 0 
            then
              loss <- loss + x.BinaryLogistic(target, true, lr)
            else 
              loss <- loss + x.BinaryLogistic(x.getNegative(target), false, lr);
          loss

        member x.HierarchicalSoftmax(target : int, lr : float) =
          let mutable loss = 0.0
          grad_.Zero()
          let binaryCode = codes.[target]
          let pathToRoot = paths.[target]
          for i = 0 to (pathToRoot.Count-1) do
            loss <- loss + x.BinaryLogistic(pathToRoot.[i], binaryCode.[i], lr);
          loss
    
        member x.ComputeOutputSoftmax() =
          output_.Mul(wo, hidden_)
          let mutable maxv = output_.Data.[0]
          let mutable z = 0.0
          for i = 0 to (osz_ - 1) do
            maxv <- max (output_.Data.[i]) maxv
          for i = 0 to (osz_ - 1) do
            output_.Data.[i] <- exp(output_.Data.[i] - maxv)
            z <- z + output_.Data.[i]
          for i = 0 to (osz_ - 1) do
            output_.Data.[i] <- output_.Data.[i] / z;

        member x.Softmax(target : int, lr : float) =
          grad_.Zero()
          x.ComputeOutputSoftmax()
          for i = 0 to (osz_ - 1) do
            let label = if i = target then 1.0 else 0.0
            let alpha = lr * (label - output_.Data.[i])
            grad_.AddRow(wo, i, alpha)
            wo.AddRow(hidden_, i, alpha);
          -Utils.log(output_.Data.[target]);

        member x.ComputeHidden(input : int[]) =
          hidden_.Zero()
          Array.iter (fun it -> hidden_.AddRow(wi, it)) input
          hidden_.Mul(1.0 / float(input.Length))


        member x.predict(input : int[], k : int, heap : SortedList<float, int>) =
          assert(k > 0)
          heap.Capacity <- k + 1
          x.ComputeHidden(input)
          if args.loss = Args.loss_name.hs
          then x.dfs(k, 2 * osz_ - 2, 0.0, heap);
          else x.findKBest(k, heap)

          //heap.Sort(fun x y -> x.Fst.CompareTo(y.Fst)) //dont need it replaced heap by sorted list

        member x.findKBest(k : int, heap : SortedList<float, int>) =
          x.ComputeOutputSoftmax()
          for i = 0 to (osz_ - 1) do
            if heap.Count = k && Utils.log(output_.Data.[i]) < heap.Last().Key
            then ()
            else heap.Add(Utils.log(output_.Data.[i]), i)
                 if heap.Count > k 
                 then heap.RemoveAt(0)//todo heap.RemoveAt(heap.Count-1)

        member x.dfs(k : int, node : int, score : float,
                              heap : SortedList<float, int>) =
              if (heap.Count = k && score < heap.Last().Key) 
              then ()
              else if (tree.[node].left = -1 && tree.[node].right = -1) 
                   then heap.Add(score, node)
                        if (heap.Count > k) 
                        then heap.RemoveAt(0)//todo heap.RemoveAt(heap.Count-1)
                   else let f = Utils.sigmoid(wo.DotRow(hidden_, node - osz_))
                        x.dfs(k, tree.[node].left, score + Utils.log(1.0 - f), heap)
                        x.dfs(k, tree.[node].right, score + Utils.log(f), heap)

        member x.update(input : int[], target : int, lr : float) =
          assert(target >= 0)
          assert(target < osz_)
          if input.Length > 0 
          then
              hidden_.Zero()
              Array.iter (fun it -> hidden_.AddRow(wi, it)) input
              hidden_.Mul(1.0 / float(input.Length))
              if args.loss = loss_name.ns
              then loss_ <- loss_ + x.NegativeSampling(target, lr)
              else if args.loss = loss_name.hs
              then loss_ <- loss_ + x.HierarchicalSoftmax(target, lr)
              else loss_ <- loss_ + x.Softmax(target, lr);
              nexamples_ <- nexamples_ + 1;

              if args.model = model_name.sup
              then grad_.Mul(1.0 / float(input.Length))
              Array.iter (fun it -> wi.AddRow(grad_, it, 1.0)) input

        member x.setTargetCounts(counts : int64[]) =
              assert(counts.Length = osz_)
              if args.loss = loss_name.ns
              then x.initTableNegatives(counts)
              if args.loss = loss_name.hs
              then x.buildTree(counts)

        member x.initTableNegatives(counts: int64[]) =
              let mutable z = 0.0
              for i = 0 to counts.Length - 1 do
                z <- z + double(counts.[i]) ** 0.5
              for i = 0 to counts.Length - 1 do
                let c = double(counts.[i]) ** 0.5
                for j = 0 to int(c * float(NEGATIVE_TABLE_SIZE) / z) - 1 do
                  negatives.Add(i)
              negatives.Sort(fun x y -> rng_.Next())

        member x.getNegative(target : int) =
              let mutable negative = 0
              let rec f () = 
                negative <- negatives.[negpos]
                negpos <- (negpos + 1) % negatives.Count
                if target = negative then f()
              f()
              negative

        member x.buildTree(counts : int64[]) =
              tree.Resize(2 * osz_ - 1)
              for i = 0 to 2 * osz_ - 2 do 
                tree.[i] <- Node(-1,-1,-1,1000000000000000L, false)


              for i = 0 to osz_ - 1 do
                let mutable x = tree.[i]
                x.count <- counts.[i]
                tree.[i] <- x
              let mutable leaf = osz_ - 1
              let mutable node = osz_
              for i = osz_ to 2 * osz_ - 2 do
                let mini = Array.zeroCreate 2
                for j = 0 to 1 do
                  if leaf >= 0 && tree.[leaf].count < tree.[node].count
                  then mini.[j] <- leaf
                       leaf <- leaf - 1
                  else mini.[j] <- node
                       node <- node + 1
                let mutable x = tree.[i]
                x.left <- mini.[0]
                x.right <- mini.[1]
                x.count <- tree.[mini.[0]].count + tree.[mini.[1]].count
                tree.[i] <- x
                let mutable x = tree.[mini.[0]]
                x.parent <- i
                x.binary <- true
                tree.[mini.[0]] <- x

              for i = 0 to osz_ - 1 do
                let path = ResizeArray<int>()
                let code = ResizeArray<bool>()
                let mutable j = i;
                while tree.[j].parent <> -1 do
                  path.Add(tree.[j].parent - osz_)
                  code.Add(tree.[j].binary)
                  j <- tree.[j].parent
                paths.Add(path)
                codes.Add(code)

        member x.getLoss() = loss_ / float(nexamples_)

