namespace FastText
module Dictionary =
    open MathNet.Numerics.Random
    open MathNet.Numerics.Distributions
    open Args
    type id_type = int
    type entry_type = word=0uy | label=1uy
    


    type Entry =
       struct
          val mutable word: String
          val mutable count: int64
          val mutable etype: entry_type
          val mutable subwords: ResizeArray<int>
          val mutable binary : bool
          new (word, count, etype, subwords, binary) =
                {word = word; count = count; etype = etype; subwords = subwords; binary = binary}
       end

    let EOS = String("</s>")
    let BOW = String("<")
    let EOW = String(">")
    let MAX_VOCAB_SIZE = 30000000
    let MAX_LINE_SIZE = 1024
    type Dictionary(args : Args) =
      let mutable size_ = 0
      let mutable nwords_ = 0
      let mutable nlabels_ = 0
      let mutable ntokens_ = 0
      let words_ = ResizeArray<Entry>()
      let pdiscard_ = ResizeArray<float>()
      let word2int_ = ResizeArray<int>(Array.create MAX_VOCAB_SIZE -1)

      member x.find(w : String) =
          let mutable h = int(w.hash() % uint32(MAX_VOCAB_SIZE))
          while word2int_.[h] <> -1 && not(words_.[word2int_.[h]].word == w) do
            h <- (h + 1) % MAX_VOCAB_SIZE
          h

      member x.add(w : String) =
          let h = x.find(w)
          ntokens_ <- ntokens_ + 1
          if word2int_.[h] = -1 
          then
            let et : entry_type = if w.StartsWith(args.label) then entry_type.label else entry_type.word
            let e = Entry(w, 1L, et, ResizeArray<_>(), false)
            
            words_.Add(e)
            word2int_.[h] <- size_
            size_ <- size_ + 1
          else
            let mutable x = words_.[word2int_.[h]]
            x.count <- x.count + 1L
            words_.[word2int_.[h]] <- x

      member x.nwords() = nwords_

      member x.nlabels() = nlabels_

      member x.ntokens() = ntokens_

      member x.getNgrams(i : int) =
          assert(i >= 0)
          assert(i < nwords_)
          words_.[i].subwords

      member x.getNgrams(word : String) =
          let mutable ngrams = ResizeArray<int>()
          let i = x.getId(word)
          if i >= 0 then ngrams <- words_.[i].subwords
          else x.computeNgrams(BOW + word + EOW, ngrams)
          ngrams

      member x.discard(id : int, rand : float) =
          assert(id >= 0)
          assert(id < nwords_)
          if args.model = model_name.sup 
          then false
          else rand > pdiscard_.[id]

      member x.getId(w : String) =
          let h = x.find(w)
          word2int_.[h]

      member x.getType(id : int) =
          assert(id >= 0)
          assert(id < size_)
          words_.[id].etype

      member x.getWord(id : int) =
          assert(id >= 0)
          assert(id < size_)
          words_.[id].word

      member x.GetStrChar(word : string, i) = uint32(word.[i])


      member x.computeNgrams(word : String, ngrams : ResizeArray<int>) =
          for i = 0 to word.Count - 1 do
            let ngram = String()
            if (word.[i] &&& 0xC0uy) = 0x80uy  then ()
            else
                let mutable j = i
                let mutable n = 1
                while j < word.Count && n <= args.maxn do
                  ngram.Add(word.[j])
                  j <- j + 1
                  while j < word.Count && (word.[j] &&& 0xC0uy) = 0x80uy do
                    ngram.Add(word.[j])
                    j <- j + 1
                  if n >= args.minn
                  then let h = int(ngram.hash()) % args.bucket //todo
                       ngrams.Add(nwords_ + int(h))
                  n <- n + 1

      member x.initNgrams() =
          for i = 0 to size_ - 1 do
            let word = BOW + words_.[i].word + EOW;
            words_.[i].subwords.Add(i);
            x.computeNgrams(word, words_.[i].subwords)

//        ' '	(0x20)	space (SPC)
//        '\t'	(0x09)	horizontal tab (TAB)
//        '\n'	(0x0a)	newline (LF)
//        '\v'	(0x0b)	vertical tab (VT)
//        '\f'	(0x0c)	feed (FF)
//        '\r'	(0x0d)	carriage return (CR)

      static member isspace(c : byte) = 
        c = 0x20uy || c = 0x09uy || c = 0x0auy || c = 0x0buy || c = 0x0cuy || c = 0x0duy

      static member readWordInt(inp : BinaryReader, word : String) = 
            let c = inp.ReadByte()
            if Dictionary.isspace(c) || c = 0uy 
            then
                if word.Empty() 
                then
                    if c = 0x0auy // \n
                    then word.AddRange(EOS.Array)
                         true
                    else Dictionary.readWordInt(inp, word)
                else
                    if c = 0x0auy // \n
                    then inp.Unget()
                    true
            else word.Add(c)
                 Dictionary.readWordInt(inp, word)

      static member inline readWord(inp : BinaryReader, word : String) = 
          word.Clear()
          try Dictionary.readWordInt(inp, word)
          with | :? System.IO.EndOfStreamException -> not <| word.Empty()
          

      member x.readFromFile(inp : BinaryReader) =
          let word = String()
          let mutable minThreshold = 1L
          while Dictionary.readWord(inp, word) do
            x.add(word)
            if ntokens_ % 1000000 = 0 && args.verbose > 1
            then printf "\rRead %d M words" (ntokens_  / 1000000)
            if size_ > (MAX_VOCAB_SIZE / 4 * 3)
            then x.threshold(minThreshold)
                 minThreshold <- minThreshold + 1L
          printfn "\rRead %d M words" (ntokens_  / 1000000)
          x.threshold(int64(args.minCount))
          x.initTableDiscard()
          x.initNgrams()
          printfn "Number of words:  %d" nwords_
          printfn "Number of labels: %d" nlabels_
          if size_ = 0
          then failwith "Empty vocabulary. Try a smaller -minCount value." 

      member x.threshold(t : int64) =
          printfn "threshold words len:  %d" words_.Count
          words_.Sort(fun e1 e2 -> if e1.etype <> e2.etype 
                                   then -e1.etype.CompareTo(e2.etype)
                                   else e1.count.CompareTo(e2.count))
          words_.RemoveAll(fun e -> e.etype = entry_type.word && e.count < t) |> ignore

          words_.ShrinkToFit()
          size_ <- 0
          nwords_ <- 0
          nlabels_ <- 0
          for i = 0 to MAX_VOCAB_SIZE - 1 do
            word2int_.[i] <- -1

          words_.ForEach(fun it ->  
                let h = x.find(it.word)
                word2int_.[h] <- size_
                size_ <- size_ + 1
                if it.etype = entry_type.word then nwords_ <- nwords_ + 1
                if it.etype = entry_type.label then nlabels_ <- nlabels_ + 1
            )

      member x.initTableDiscard() =
          pdiscard_.Resize(size_)
          for i = 0 to size_ - 1 do
            let f = float(words_.[i].count) / float(ntokens_)
            pdiscard_.[i] <- sqrt(args.t / f) + args.t / f

      member x.getCounts(etype : entry_type) =
          let counts = ResizeArray<int64>()
          for w in words_ do
            if w.etype = etype then counts.Add(w.count)
          counts

      member x.addNgrams(line : ResizeArray<int>, n : int) =
          let line_size = line.Count
          for i = 0 to line_size - 1 do
            let mutable h = line.[i]
            for j = i + 1 to min (line_size - 1) (i + n) do
              h <- h * 116049371 + line.[j]
              line.Add(nwords_ + (h % int(args.bucket)))

      member x.cycle(uniform : ContinuousUniform,
                     inp : BinaryReader, 
                     words : ResizeArray<int>,
                     labels : ResizeArray<int>,
                     token : String, 
                     ntokens :int) = 
            if not (Dictionary.readWord(inp, token)) then ntokens
            else if token == EOS then ntokens
            else let wid = x.getId(token)
                 if wid < 0 then x.cycle(uniform, inp, words, labels, token, ntokens)
                 else let etype = x.getType(wid)
                      if etype = entry_type.word &&  not (x.discard(wid, uniform.Sample()))
                      then words.Add(wid)
                      if etype = entry_type.label 
                      then labels.Add(wid - nwords_)
                      if words.Count > MAX_LINE_SIZE && args.model <> model_name.sup
                      then ntokens + 1
                      else x.cycle(uniform, inp, words, labels, token, ntokens + 1)

      member x.getLine(inp : BinaryReader, 
                       words : ResizeArray<int>,
                       labels : ResizeArray<int>,
                       rng : Mcg31m1)=
          let uniform = ContinuousUniform(0., 1., rng)
          let token = String()
          words.Clear()
          labels.Clear()
//          if inp.eof() 
//          then // todo check inp.clear()
//               inp.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore//todo

          x.cycle(uniform, inp, words, labels, token, 0)

      member x.getLabel(lid : int) =
          assert(lid >= 0)
          assert(lid < nlabels_)
          words_.[lid + nwords_].word

      member x.save(out : System.IO.BinaryWriter) =
          out.Write(size_)
          out.Write(nwords_)
          out.Write(nlabels_)
          out.Write(ntokens_)
          for i = 0 to size_ - 1 do
            let e = words_.[i]
            out.Write(e.word.Array.ToArray())
            out.Write(0uy)
            out.Write(e.count)
            out.Write(byte(e.etype))

      member x.load(inp : System.IO.BinaryReader) =
          words_.Clear()
          for i = 0 to MAX_VOCAB_SIZE - 1 do
            word2int_.[i] <- -1
          let size_ = inp.ReadInt32()
          let nwords_ = inp.ReadInt32()
          let nlabels_ = inp.ReadInt32()
          let ntokens_ = inp.ReadInt32()
          for i = 0 to size_ - 1 do
            let c = inp.ReadByte()
            let word = String()
            while c <> 0uy do
              word.Add(c)
            let count = inp.ReadInt64()
            let etype : entry_type = LanguagePrimitives.EnumOfValue <| inp.ReadByte()
            words_.Add(Entry(word, count, etype, ResizeArray(), false))
            word2int_.[x.find(word)] <- i
          x.initTableDiscard()
          x.initNgrams()


