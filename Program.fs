open FinvizScraper

let screener = Config.screeners[0]

let screenerResults = FinvizClient.getResults screener.url

let breakdowns =
    Config.breakdowns
    |> List.map (fun x -> (x.name, (screenerResults |> Processing.resultBreakdown x.breakdown)))

let html =
    screenerResults
    |> Rendering.renderScreenerResultsAsHtml 
        screener.name
        breakdowns

System.IO.File.WriteAllText("index.html", html)