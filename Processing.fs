namespace FinvizScraper

module Processing =

    let resultBreakdown groupBy (sequence:seq<ScreenerResult>) =
        sequence
        |> Seq.groupBy groupBy
        |> Seq.sortBy (fun a -> 
            let list = Seq.toList (snd(a))
            list.Length * -1
        )
        |> Seq.map (fun a -> (fst(a).ToString(), Seq.toList (snd(a))))