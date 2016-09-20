namespace FastText

module FastTextM =
    open System.IO
    open Matrix
    open Dictionary
    open Args
    open Model
    open System.Collections.Generic
    open System.IO
    open System.Diagnostics
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions
    open System.Threading
    type FastText() =  
        let mutable args_ = Args()
        let mutable dict_ = Dictionary(args_)
        let mutable input_ = Matrix()
        let mutable output_ = Matrix()
        let mutable model_ = Model(input_, output_, args_, null)
        let mutable tokenCount = 0L //atomic todo
        let mutable start = Stopwatch.StartNew() // todo clock_t
        let EXIT_FAILURE  = 1

        member x.getVector(vec : Vector, word : String) =
          let ngrams = dict_.getNgrams(word)
          vec.Zero()
          for it in ngrams do
             vec.AddRow(input_, it)
          if ngrams.Count > 0 
          then vec.Mul(1.0 / float(ngrams.Count))

        member x.saveVectors() =
          use ofs = try new StreamWriter(args_.output + ".vec") 
                    with ex -> failwith "Error opening file for saving vectors."
          ofs.WriteLine(sprintf "%d %d" (dict_.nwords()) (args_.Dim))
          let vec = Vector(args_.Dim)
          for i = 0 to dict_.nwords() - 1 do
            let word = dict_.getWord(i)
            x.getVector(vec, word)
            ofs.WriteLine(sprintf "%s %A" (word.ToString()) vec)
          ofs.Close()

        member x.saveModel() =
          use ofs = try new BinaryWriter(File.Open(args_.output + ".bin", FileMode.Create)) 
                    with ex -> failwith "Model file cannot be opened for saving!"

          args_.save(ofs)
          dict_.save(ofs)
          input_.save(ofs)
          output_.save(ofs)
          ofs.Close()

        member x.loadModel(filename : string) =
          use ifs = try new BinaryReader(File.Open(filename + ".bin", FileMode.Open)) 
                    with ex -> failwith "Model file cannot be opened for loading!"
          args_ <- Args()
          dict_ <- Dictionary(args_)
          input_ <- Matrix()
          output_ <- Matrix()
          args_.load(ifs)
          dict_.load(ifs)
          input_.load(ifs)
          output_.load(ifs)
          model_ <- Model(input_, output_, args_, 0)
          if args_.model = model_name.sup
          then model_.setTargetCounts(dict_.getCounts(entry_type.label).ToArray())
          else model_.setTargetCounts(dict_.getCounts(entry_type.word).ToArray())
          ifs.Close()

        member x.printInfo(progress : float, loss : float) =
          let t = float(start.Elapsed.TotalSeconds) 
          let wst = float(tokenCount) / t
          let lr = args_.lr * (1.0 - progress)
          let eta = int(t / progress * (1. - progress) / float(args_.thread))
          let etah = eta / 3600
          let etam = (eta - etah * 3600) / 60
          printf "\rProgress: %.1f" (100. * progress)
          printf "  words/sec/thread: %.0f" wst
          printf "  lr: %.6f" lr;
          printf "  loss: %.6f" loss
          printf "  eta: %dh %dm" etah etam

        member x.supervised(model : Model, 
                            lr : float,
                            line : ResizeArray<int>,
                            labels : ResizeArray<int>) =
          if labels.Count = 0 || line.Count = 0 then ()
          else let uniform = DiscreteUniform(0, labels.Count - 1, model.rng)
               let i = uniform.Sample()
               model.update(line.ToArray(), labels.[i], lr)

        member x.cbow(model : Model, lr : float, line : ResizeArray<int>) =
          let bow =  ResizeArray<int>()
          let uniform = DiscreteUniform (1, args_.ws, model.rng)
          for w = 0 to line.Count - 1 do
            let boundary = uniform.Sample()
            bow.Clear()
            for c = -boundary to boundary do
              if c <> 0 && w + c >= 0 && w + c < line.Count
              then let ngrams = dict_.getNgrams(line.[w + c])
                   bow.AddRange(ngrams)
            model.update(bow.ToArray(), line.[w], lr)

        member x.skipgram(model : Model, lr : float, line : ResizeArray<int>) =
          let uniform = DiscreteUniform(1, args_.ws,model.rng)
          for w = 0 to line.Count - 1 do
            let boundary = uniform.Sample()
            let ngrams = dict_.getNgrams(line.[w])
            for c = -boundary to boundary do
              if c <> 0 && w + c >= 0 && w + c < line.Count
              then model.update(ngrams.ToArray(), line.[w + c], lr);

        member x.test(filename : string, k : int) =
          let mutable nexamples = 0
          let mutable nlabels = 0
          let mutable precision = 0.0
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          use ifs = try new BinaryReader(File.Open(filename, FileMode.Open))
                    with ex -> failwith "Test file cannot be opened!"
          
          while ifs.NotEOF() do
            dict_.getLine(ifs, line, labels, model_.rng) |> ignore
            dict_.addNgrams(line, args_.wordNgrams);
            if (labels.Count > 0 && line.Count > 0) 
            then
              let predictions = SortedList<float, int>()
              model_.predict(line.ToArray(), k, predictions)
              for it in predictions do
                if labels.Contains(it.Value) 
                then precision <- precision + 1.0
              nexamples <- nexamples + 1
              nlabels <- nlabels + labels.Count
          ifs.Close()
          printfn "P@%d:%.3f" k (precision / float(k * nexamples)) 
          printfn "R@%d:%.3f" k (precision / float(nlabels))
          printfn "Number of examples: %d" nexamples

        member x.predict(filename : string, k : int, print_prob : bool) =
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          use ifs = try new BinaryReader(File.Open(filename, FileMode.Open))
                    with ex -> failwith "Test file cannot be opened!"
          
          while ifs.NotEOF() do
            dict_.getLine(ifs, line, labels, model_.rng) |> ignore // todo
            dict_.addNgrams(line, args_.wordNgrams)
            if line.Count = 0 
            then printfn "n/a"
            else
                let predictions = SortedList<float, int>()
                model_.predict(line.ToArray(), k, predictions)
                let fstK = predictions.Keys.[0]
                for it in predictions do
                  if it.Key <> fstK then printf " "
                  printf "%s" (dict_.getLabel(it.Value).ToString())
                  if print_prob then printf " %A" <| exp(it.Key)
                printfn ""
          ifs.Close()

        member x.wordVectors() =
          let word = String()
          let vec = Vector(args_.Dim)
          use cin = new BinaryReader(System.Console.OpenStandardInput())
          let word = String()
          while cin.NotEOF() do
            let c = cin.ReadByte()
            if c = 0uy 
            then x.getVector(vec, word)
                 printfn "%s %A" (word.ToString()) vec
                 word.Clear()
            else word.Add(c)

        member x.textVectors() =
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          let vec = Vector(args_.Dim)
          use cin = new BinaryReader(System.Console.OpenStandardInput())
          while cin.NotEOF() do
            dict_.getLine(cin, line, labels, model_.rng) |> ignore//todo
            dict_.addNgrams(line, args_.wordNgrams)
            vec.Zero()
            for it in line do
              vec.AddRow(input_, it)
            if line.Count > 0
            then vec.Mul(1.0 / float(line.Count))
            printfn "%A" vec

        member x.printVectors() =
          if args_.model = model_name.sup 
          then x.textVectors()
          else x.wordVectors()

        member x.trainThread(threadId : int) =
          use ifs = new BinaryReader(File.Open(args_.input, FileMode.Open))
          Utils.seek(ifs, int64(threadId) * Utils.size(ifs) / int64(args_.thread)) |> ignore //todo

          let model = Model(input_, output_, args_, threadId)
          if args_.model = model_name.sup
          then model.setTargetCounts(dict_.getCounts(entry_type.label).ToArray())
          else model.setTargetCounts(dict_.getCounts(entry_type.word).ToArray())

          let ntokens = dict_.ntokens()
          let mutable localTokenCount = 0
          let line = ResizeArray<int>()
          let labels = ResizeArray<int>()
          while tokenCount < int64(args_.epoch * ntokens) do
            let progress = float(tokenCount) / float(args_.epoch * ntokens)
            let lr = args_.lr * (1.0 - progress)
            localTokenCount <- localTokenCount + dict_.getLine(ifs, line, labels, model.rng)
            if args_.model = model_name.sup
            then
              dict_.addNgrams(line, args_.wordNgrams)
              x.supervised(model, lr, line, labels)
            else if args_.model = model_name.cbow
            then x.cbow(model, lr, line)
            else if args_.model = model_name.sg
            then x.skipgram(model, lr, line)
            if localTokenCount > args_.lrUpdateRate
            then
              tokenCount <- tokenCount + int64(localTokenCount)
              localTokenCount <- 0
              if threadId = 0 && args_.verbose > 1
              then x.printInfo(progress, model.getLoss())
          if threadId = 0 
          then x.printInfo(1.0, model.getLoss())
               printfn ""
          ifs.Close()

        member x.train(args : Args) =
          args_ <- args
          dict_ <- Dictionary(args_)
          use ifs = try new BinaryReader(File.Open(args_.input, FileMode.Open))
                    with ex -> failwith "Input file cannot be opened!"
          
          dict_.readFromFile(ifs)
          ifs.Close()

          input_ <- Matrix(dict_.nwords() + int(args_.bucket), args_.Dim)
          if args_.model = model_name.sup
          then output_ <- Matrix(dict_.nlabels(), args_.Dim)
          else output_ <- Matrix(dict_.nwords(), args_.Dim)
          input_.Uniform(1.0 / float(args_.Dim))
          output_.Zero()

          start <- Stopwatch.StartNew()
          tokenCount <- 0L
          let threads = ResizeArray<Thread>()
          for i = 0 to args_.thread - 1 do
            let t = Thread(fun () -> x.trainThread i)
            t.Start()
            threads.Add(t)
          for it in threads do
            it.Join()
          model_ <- Model(input_, output_, args_, 0)

          x.saveModel()
          if args_.model <> model_name.sup 
          then x.saveVectors()

    let printUsage() =
        printf "usage: fasttext <command> <args>\n\n"
        printf "The commands supported by fasttext are:\n\n"
        printf "  supervised          train a supervised classifier\n"
        printf "  test                evaluate a supervised classifier\n"
        printf "  predict             predict most likely labels\n"
        printf "  predict-prob        predict most likely labels with probabilities\n"
        printf "  skipgram            train a skipgram model\n"
        printf "  cbow                train a cbow model\n"
        printf "  print-vectors       print vectors given a trained model\n"
        printfn ""

    let printTestUsage() =
        printf "usage: fasttext test <model> <test-data> [<k>]\n\n"
        printf "  <model>      model filename\n"
        printf "  <test-data>  test data filename\n"
        printf "  <k>          (optional; 1 by default) predict top k labels\n"
        printfn ""

    let printPredictUsage() =
        printf "usage: fasttext predict[-prob] <model> <test-data> [<k>]\n\n"
        printf "  <model>      model filename\n"
        printf "  <test-data>  test data filename\n"
        printf "  <k>          (optional; 1 by default) predict top k labels\n"
        printfn ""

    let printPrintVectorsUsage() =
        printf "usage: fasttext print-vectors <model>\n\n"
        printf "  <model>      model filename\n"
        printfn ""
            

    let test(argv : string[]) =
        let k = if  argv.Length = 4 
                then 1
                else if argv.Length = 5
                then int(argv.[4])
                else printTestUsage()
                     failwith ""
        let fasttext = FastText()
        fasttext.loadModel(argv.[2])
        fasttext.test(argv.[3], k)

    let predict(argv : string[]) =
        let k = if argv.Length = 4 then 1
                else if argv.Length = 5 then int(argv.[4])
                else printPredictUsage()
                     failwith("")
        let print_prob = argv.[1] = "predict-prob"
        let fasttext = FastText()
        fasttext.loadModel(argv.[2])
        fasttext.predict(argv.[3], k, print_prob)

    let printVectors(argv : string[]) =
        if argv.Length <> 3
        then printPrintVectorsUsage()
             failwith ""
          
        let fasttext = FastText()
        fasttext.loadModel(argv.[2])
        fasttext.printVectors()

    let train(argv : string[]) =
        let a = Args()
        a.parseArgs(argv)
        let fasttext = FastText()
        fasttext.train(a)

    let main(argv : string[]) =
        try
            let argv = Array.append [|"fastText"|] argv
//            printf "%A" argv
            if argv.Length < 2
            then printUsage();
                 failwith ""
            let command = argv.[1]
            if command = "skipgram" || command = "cbow" || command = "supervised"
            then train(argv)
            else if command = "test" then test(argv)
            else if command = "print-vectors" then printVectors(argv)
            else if command = "predict" || command = "predict-prob" then predict(argv)
            else printUsage()
                 failwith ""
            0
        with ex -> eprintfn "%s" ex.Message
                   1
          

