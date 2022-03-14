open FinvizScraper

let runAndSaveScreener (screener:ScreenerInput) =

    System.Console.WriteLine("Screener: " + (screener.name))

    let screenerResults = FinvizClient.getResults screener.url

    let breakdowns =
        Config.breakdowns
        |> List.map (
            fun x -> 
                (x.name, (screenerResults |> Processing.resultBreakdown x.breakdown))
            )

    let html =
        screenerResults
        |> Rendering.renderScreenerResultsAsHtml 
            screener
            breakdowns

    System.IO.File.WriteAllText(screener.filename, html)

let args = System.Environment.GetCommandLineArgs()

let defaultConfigPath = "config.json"
let configPath =
    match args with
    | [||] -> defaultConfigPath
    | [|_|] -> defaultConfigPath
    | _ -> args[1]

let config = Config.readConfig configPath

let screeners = config.screeners

screeners 
    |> Seq.iter runAndSaveScreener

let indexPage = 
    screeners
    |> Rendering.renderIndex

System.IO.File.WriteAllText(config.outputPath, indexPage)