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

let shouldRunIndustryUpdates() =
    let args = Environment.GetCommandLineArgs()
    args |> Array.toList |> List.tryFind (fun arg -> arg = "--update-industries")

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

// let screenerResults =
//     config.screeners 
//     |> Seq.map fetchScreenerResults
//     |> Seq.toList

match config.dbConnectionString with
| null -> 
    Console.WriteLine("No db connection string found in config... not storing the results in db")
| value -> 
    value |> Storage.configureConnectionString
    // screenerResults
    //     |> saveToDb config

match shouldRunIndustryUpdates() with
| Some _ ->         
    Storage.getIndustries()
        |> List.toSeq
        |> Seq.map (fun sector -> 
            let (above,below) = sector |> FinvizClient.getResultCountForIndustryAboveAndBelow20
            (sector,above,below)
        )
        |> Seq.iter (fun result ->  
            Storage.saveIndustryUpdates (FinvizConfig.getRunDate()) result |> ignore
        )
| None -> ()