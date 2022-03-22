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

let saveAsHtml config screenerResults =
    screenerResults
    |> Seq.map generateAndAppendHtml
    |> Seq.iter saveToFile

    let indexHtml = Rendering.createIndexPage screenerResults
    System.IO.File.WriteAllText(config.outputPath, indexHtml)

let saveToDb (screenerResults:list<ScreenerInput * 'a>) =
    System.Console.WriteLine("Save to db in program... " + screenerResults.Length.ToString())
    let date = FinvizConfig.getRunDate()
    screenerResults
    |> Seq.iter (fun x -> Storage.saveScreenerResults date x)

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
    |> Seq.map fetchScreenerResults
    |> Seq.toList

screenerResults
    |> saveAsHtml config

screenerResults
    |> saveToDb