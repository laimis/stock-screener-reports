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

let runSMAUpdates() =
    containsArgument "--industry-sma-breakdowns"

let runScreeners() =
    containsArgument "--screeners"

let runTestReports() =
    containsArgument "--test-reports"

let fetchScreenerResults input =
    Console.WriteLine("Processing " + input.name)
    Console.WriteLine("fetching results...")
    let results = FinvizClient.getResults input.url
    (input,results)

let saveToFile (filepath:string) content =
    let directory = IO.Path.GetDirectoryName(filepath)
    IO.Directory.CreateDirectory(directory) |> ignore
    IO.File.WriteAllText(filepath,content)

let saveToDb (screenerResults:list<Screener * 'a>) =

    System.Console.WriteLine("Saveing to db " + screenerResults.Length.ToString() + " screener results")
    let date = FinvizConfig.getRunDate()
    screenerResults
    |> List.iter (fun x -> Storage.saveScreenerResults date x)
    screenerResults

let config = readConfig()

match config.dbConnectionString with
| null -> 
    Console.Error.WriteLine("No db connection string found in config...")
    Environment.Exit(-1)
| value -> 
    value |> Storage.configureConnectionString
    value |> Reports.configureConnectionString

match runScreeners() with
| true ->
    
    let screenerResults =
        Storage.getScreeners()
        |> List.map fetchScreenerResults
        |> saveToDb
    
    Storage.saveJobStatus ScreenerJob (DateTimeOffset.UtcNow) Success $"Ran {screenerResults.Length} screeners" |> ignore

| false -> ()


match runSMAUpdates() with
| true ->     
    let date = FinvizConfig.getRunDate()
    
    // pull above and below 20 and 200 for each industry, and store the results
    let industriesUpdated =
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
                Storage.saveIndustrySMABreakdowns date r |> ignore
            )
        )
        |> Seq.length

    Storage.updateSMABreakdowns date 20 |> ignore
    Storage.updateSMABreakdowns date 200 |> ignore
    
    Storage.saveJobStatus IndustryTrendsJob (DateTimeOffset.UtcNow) Success $"Updated trends for {industriesUpdated} industries" |> ignore

| false -> ()


match runTestReports() with
| true -> 

    Console.WriteLine("Enter industry name:")

    let input = Console.ReadLine()

    let industry = 
        match input with
        | "" ->
            Console.Error.WriteLine("No industry name entered, defaulting to 'Consulting Services'")
            "Consulting Services"
        | value ->
            value


    let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry 20 industry

    let mutable latestValue = Option<float>.None
    let mutable direction = Option<int>.None
    let mutable streak = 1
    let mutable endReached = false
    let mutable firstDate = DateTime.UtcNow
    let mutable lastDate = DateTime.UtcNow

    smaBreakdowns
        |> List.rev
        |> List.iter (fun x ->

            if endReached then
                ()
            else
                match (latestValue, direction) with
                // case where we are seeing the first value
                | (None, None) -> 
                    latestValue <- Some x.breakdown.percentAbove
                    firstDate <- x.breakdown.date

                // case where we are seeing the second value
                | (Some v, None) -> (
                    if x.breakdown.percentAbove > latestValue.Value then
                        direction <- Some -1
                    else
                        direction <- Some 1
                    )

                // case where we are now iterating
                | (Some _, Some _) ->
                    let newDirection =
                        match x.breakdown.percentAbove > latestValue.Value with
                        | true -> -1
                        | false -> 1
                    
                    if newDirection = direction.Value then
                        streak <- streak + 1
                        latestValue <- Some x.breakdown.percentAbove
                        lastDate <- x.breakdown.date
                    else
                        endReached <- true

                | (None, Some _) -> raise (new Exception("should not happen where latestValue is None and direction is not"))
        )

    let directionString = 
        match direction with
        | None -> "None"
        | Some d ->
            match d with
            | -1 -> "down"
            | 1 -> "up"
            | _ -> "unknown"

    Console.WriteLine($"{industry} {20} days sma streak: {streak} day {directionString} from {firstDate} to {lastDate}")

| false -> ()