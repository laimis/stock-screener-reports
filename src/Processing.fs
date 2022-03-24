namespace FinvizScraper

module Processing =

    let resultBreakdown groupBy (sequence:seq<ScreenerResult>) =
        sequence
        |> Seq.groupBy groupBy
        |> Seq.sortBy (fun a -> 
            let (_, list) = a
            (Seq.length list) * -1
        )
        |> Seq.map (fun a -> 
            let (name, list) = a
            (name, Seq.toList list))