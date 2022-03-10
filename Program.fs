open FinvizScraper

let runAndSaveScreener screener =
    let screenerResults = FinvizClient.getResults screener.url

    let breakdowns =
        Config.breakdowns
        |> List.map (fun x -> (x.name, (screenerResults |> Processing.resultBreakdown x.breakdown)))

    let html =
        screenerResults
        |> Rendering.renderScreenerResultsAsHtml 
            screener.name
            breakdowns

    System.IO.File.WriteAllText(screener.filename, html)

Config.screeners
    |> List.iter runAndSaveScreener