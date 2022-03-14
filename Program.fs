open FinvizScraper
open System

let fetchScreenerResults (input:ScreenerInput) =
    Console.WriteLine("Processing " + input.name)
    Console.WriteLine("fetching results...")
    let results = FinvizClient.getResults input.url
    (input,results)

let generateAndAppendHtml (input:ScreenerInput,results:list<ScreenerResult>) =
    Console.WriteLine("generating html... ")
    Rendering.renderResultsAsHtml input Config.breakdowns results

let saveToFile (input,results,html) =
    System.IO.File.WriteAllText(input.filename, html)
    (input,results)

let saveIndex outputPath screenerResults =
    let indexHtml = Rendering.createIndexPage screenerResults
    System.IO.File.WriteAllText(outputPath, indexHtml)

let args = Environment.GetCommandLineArgs()
let defaultConfigPath = "config.json"
let configPath =
    match args with
    | [||] -> defaultConfigPath
    | [|_|] -> defaultConfigPath
    | _ -> args[1]

let config = Config.readConfig configPath

let screenerResults =
    config.screeners 
    |> List.map fetchScreenerResults
    |> List.map generateAndAppendHtml
    |> List.map saveToFile

saveIndex config.outputPath screenerResults