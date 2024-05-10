module StockScreenerReports.FinvizClient.StockAnalysisClient

open StockScreenerReports.Core

let mutable outputFunc = (fun _ -> ())

let private fetchUrl (url:string) =
        // make ure that we sleep a bit before each request
        url |> outputFunc 
        let web = HtmlAgilityPack.HtmlWeb()
        web.Load(url)
        
let getCorporateActions() =
        
    let doc = fetchUrl "https://stockanalysis.com/actions/"
    
    let tableRows =
        doc.DocumentNode.Descendants("table")
        |> Seq.find (fun t -> t.HasClass("svelte-1yyv6eq"))
        |> _.Descendants("tbody")
        |> Seq.head
        |> _.Descendants("tr")

    tableRows
    |> Seq.map (fun row ->
        let cells = row.Descendants("td") |> Seq.toList
        {
            Date = cells.[0].InnerText.Trim()
            Symbol = cells.[1].InnerText.Trim()
            Type = cells.[2].InnerText.Trim()
            Action = cells.[3].InnerText.Trim()
        })
    |> Seq.toList