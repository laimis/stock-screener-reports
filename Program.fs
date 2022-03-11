open FinvizScraper

let runAndSaveScreener (screener:ScreenerInput) =

    System.Console.WriteLine("Screener: " + (screener.name))

    let screenerResults = FinvizClient.getResults screener.url

    let breakdowns =
        Config.breakdowns
        |> List.map (fun x -> (x.name, (screenerResults |> Processing.resultBreakdown x.breakdown)))

    let html =
        screenerResults
        |> Rendering.renderScreenerResultsAsHtml 
            screener
            breakdowns

    System.IO.File.WriteAllText(screener.filename, html)

Config.screeners
    |> List.iter runAndSaveScreener

let indexPage = 
    Config.screeners
    |> Rendering.renderIndex

System.IO.File.WriteAllText("output.html", indexPage)