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

let saveToFile (filepath:string) content =
    let directory = IO.Path.GetDirectoryName(filepath)
    IO.Directory.CreateDirectory(directory) |> ignore
    IO.File.WriteAllText(filepath,content)


let saveAsHtml config screenerResults =
    screenerResults
    |> Seq.map generateAndAppendHtml
    |> Seq.iter (fun (input,_,html) -> html |> saveToFile input.filename)

    Rendering.createIndexPage screenerResults
    |> saveToFile config.outputPath

let saveToDb config (screenerResults:list<ScreenerInput * 'a>) =
    match config.dbConnectionString with
        | null -> 
            Console.WriteLine("No db connection string found in config... not storing the results in db")
        | value ->
            Storage.storeCnn value
            System.Console.WriteLine("Saveing to db " + screenerResults.Length.ToString() + " screener results")
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
    |> saveToDb config