open System
open StockScreenerReports.Core
open StockScreenerReports.FinvizClient
open StockScreenerReports.Storage

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

let runTrendsMigration() =
    containsArgument "--trends-migration"

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
    let date = Utils.getRunDate()
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

    let earnings = FinvizClient.getEarnings()

    earnings
        |> List.iter (fun x ->
            let (ticker,earningsTime) = x
            Storage.saveEarningsDate ticker (Utils.getRunDate()) earningsTime |> ignore
        )
    
    Storage.saveJobStatus ScreenerJob (DateTimeOffset.UtcNow) Success $"Ran {screenerResults.Length} screeners" |> ignore

| false -> ()


match runSMAUpdates() with
| true ->     
    let date = Utils.getRunDate()
    
    // pull above and below 20 and 200 for each industry, and store the results
    let knownIndustries = Storage.getIndustries()

    let industriesUpdated =
        knownIndustries
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

    let trendsUpdated =
        knownIndustries
        |> Seq.map (fun industry -> 
            
            [20; 200]
            |> List.map(fun days -> 
                
                let breakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry days FinvizConfig.dayRange

                let trend = breakdowns |> TrendsCalculator.calculateForIndustry

                let lastBreakdown = breakdowns |> List.last

                Console.WriteLine($"Saving industry {industry} trend: {trend.direction} {trend.streak} days with change of {trend.change}")

                Storage.updateIndustryTrend lastBreakdown trend
            )
            |> List.sum
        )
        |> Seq.sum
    
    Storage.saveJobStatus
        IndustryTrendsJob
        (DateTimeOffset.UtcNow)
        Success
        $"Updated sma breakdowns for {industriesUpdated} industries and calculated {trendsUpdated} trends"
    |> ignore

| false -> ()

match runTrendsMigration() with
| true ->
    let knownIndustries = Storage.getIndustries()

    let startDate = DateTime.Parse("2022-09-01T00:00:00") |> Utils.convertToDateString
    let endDate = DateTime.UtcNow |> Utils.convertToDateString

    knownIndustries
    |> Seq.iter (fun industry -> 
        [20; 200]
        |> List.iter(fun days -> 

            let breakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustryAndDateRange days startDate endDate

            breakdowns
            |> List.windowed (FinvizConfig.dayRange)
            |> List.iter( fun window ->
                let trend = window |> TrendsCalculator.calculateForIndustry
                let last = window |> List.last
                Console.WriteLine($"Saving industry {industry} {last.breakdown.date} trend: {trend.direction} {trend.streak} days with change of {trend.change}")
                Storage.updateIndustryTrend last trend |> ignore
            )
        )
    ) |> ignore
    
| false ->
    ()


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


    let smaBreakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry 20 FinvizConfig.dayRange

    let trend = smaBreakdowns |> TrendsCalculator.calculateForIndustry

    Console.WriteLine($"{industry} {20} days sma streak: {trend.streak} day {trend.direction} with change of {trend.change}")

| false -> ()