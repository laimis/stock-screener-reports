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

    Console.WriteLine("Reading config from " + configPath)
    System.Text.Json.JsonSerializer.Deserialize<FinvizConfig>(
        System.IO.File.ReadAllText(configPath)
    )

let containsArgument toFind =
    let args = Environment.GetCommandLineArgs()
    let m = args |> Array.toList |> List.tryFind (fun arg -> arg = toFind)
    match m with
    | None -> false
    | Some _ -> true

let runIndustryUpdates() =
    containsArgument "--industry-updates"

let runScreeners() =
    containsArgument "--screeners"

let fetchScreenerResults (input:ScreenerInput) =
    Console.WriteLine("Processing " + input.name)
    Console.WriteLine("fetching results...")
    let results = FinvizClient.getResults input.url
    (input,results)

let saveToFile (filepath:string) content =
    let directory = IO.Path.GetDirectoryName(filepath)
    IO.Directory.CreateDirectory(directory) |> ignore
    IO.File.WriteAllText(filepath,content)

let saveToDb (screenerResults:list<ScreenerInput * 'a>) =

    System.Console.WriteLine("Saveing to db " + screenerResults.Length.ToString() + " screener results")
    let date = FinvizConfig.getRunDate()
    screenerResults
    |> Seq.iter (fun x -> Storage.saveScreenerResults date x)

let config = readConfig()

match config.dbConnectionString with
| null -> 
    Console.Error.WriteLine("No db connection string found in config...")
    Environment.Exit(-1)
| value -> 
    value |> Storage.configureConnectionString

match runScreeners() with
| true ->
    let screenerResults =
        config.screeners 
        |> Seq.map fetchScreenerResults
        |> Seq.toList

    screenerResults
            |> saveToDb

    Storage.saveJobStatus IndustryTrendsJob (DateTime.UtcNow) Success $"Ran {screenerResults.Length} screeners" |> ignore

| false -> ()

match runIndustryUpdates() with
| true ->     
    let updateCount =
        Storage.getIndustries()
        |> List.toSeq
        |> Seq.map (fun industry -> 
            let (above20,below20) = industry |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA 20
            let (above200,below200) = industry |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA 200
            [(industry,20,above20,below20); (industry,200,above200,below200)]
        )
        |> Seq.map (fun result ->
            result
            |> List.iter(fun r ->
                let (industry, days, _, _) = r
                Console.WriteLine($"Saving industry {industry} {days} days sma")
                Storage.saveIndustryUpdates (FinvizConfig.getRunDate()) r |> ignore
            )
        )
        |> Seq.length
    
    Storage.saveJobStatus IndustryTrendsJob (DateTime.UtcNow) Success $"Updated trends for {updateCount} industries" |> ignore

| false -> ()