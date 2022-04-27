open System
open FinvizScraper.Core
open FinvizScraper.FinvizClient
open FinvizScraper.Storage

let readConfig() =
    let args = Environment.GetCommandLineArgs()
    let defaultConfigPath = "config.json"
    let configPath =
        match args with
        | [||] -> defaultConfigPath
        | [|_|] -> defaultConfigPath
        | _ -> args[1]
    System.Text.Json.JsonSerializer.Deserialize<FinvizConfig>(
        System.IO.File.ReadAllText(configPath)
    )

let fetchScreenerResults (input:ScreenerInput) =
    Console.WriteLine("Processing " + input.name)
    Console.WriteLine("fetching results...")
    let results = FinvizClient.getResults input.url
    (input,results)

let saveToFile (filepath:string) content =
    let directory = IO.Path.GetDirectoryName(filepath)
    IO.Directory.CreateDirectory(directory) |> ignore
    IO.File.WriteAllText(filepath,content)

let saveToDb config (screenerResults:list<ScreenerInput * 'a>) =
    match config.dbConnectionString with
        | null -> 
            Console.WriteLine("No db connection string found in config... not storing the results in db")
        | value ->
            Storage.configureConnectionString value
            System.Console.WriteLine("Saveing to db " + screenerResults.Length.ToString() + " screener results")
            let date = FinvizConfig.getRunDate()
            screenerResults
            |> Seq.iter (fun x -> Storage.saveScreenerResults date x)

let config = readConfig()

let screenerResults =
    config.screeners 
    |> Seq.map fetchScreenerResults
    |> Seq.toList

screenerResults
    |> saveToDb config

let (above,below) = 
    "airlines"
    |> FinvizClient.getResultCountForIndustryAboveAndBelow20

Console.WriteLine($"{above} / {above + below}")