open System
open StockScreenerReports.Core
open StockScreenerReports.FinvizClient
open StockScreenerReports.Storage
open StockScreenerReports.Web.Shared.Utils

let updateStatus (message:string) =
    Console.WriteLine(message)

let readConfig() =
    let args = Environment.GetCommandLineArgs()
    let defaultConfigPath = "config.json"
    let configPath =
        match args with
        | [||] -> defaultConfigPath
        | [|_|] -> defaultConfigPath
        | _ -> args[1]

    Console.WriteLine("Reading config from " + configPath)
    System.Text.Json.JsonSerializer.Deserialize<ReportsConfig>(
        System.IO.File.ReadAllText(configPath)
    )

let containsArgument toFind =
    let args = Environment.GetCommandLineArgs()
    let m = args |> Array.toList |> List.tryFind (fun arg -> arg = toFind)
    match m with
    | None -> false
    | Some _ -> true

let now = ReportsConfig.now()

Console.WriteLine("Run date: " + now.ToString())


let isTradingDay = ReportsConfig.now() |> ReportsConfig.isTradingDay

let runSMAUpdates() =
    isTradingDay && containsArgument "--industry-sma-breakdowns"

let runScreeners() =
    isTradingDay && containsArgument "--screeners"

let runTestReports() =
    containsArgument "--test-reports"

let runCyclesMigration() =
    containsArgument "--cycles-migration"

let runCountriesJob() =
    containsArgument "--countries-job"
    
let config = readConfig()

updateStatus "Config read"

match config.dbConnectionString with
| null -> 
    Console.Error.WriteLine("No db connection string found in config...")
    Environment.Exit(-1)
| value -> 
    value |> Storage.configureConnectionString
    value |> Reports.configureConnectionString

updateStatus "Configured db connection string"

updateStatus ("Trading day: " + isTradingDay.ToString())

match runScreeners() with
| true ->
    let logger = new DummyLogger()
    StockScreenerReports.Web.Services.screenerRun (logger)
    StockScreenerReports.Web.Services.earningsRun (logger)
| false -> ()


match runSMAUpdates() with
| true ->     
    StockScreenerReports.Web.Services.trendsRun (new DummyLogger())
| false -> ()

match runCyclesMigration() with
| true ->
    let knownIndustries = Storage.getIndustries()

    knownIndustries
    |> Seq.iter (fun industry -> 
        
        let range = ReportsConfig.dateRangeAsStrings()
        let breakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry SMA20 range
        let trendWithCycle = breakdowns |> TrendsCalculator.calculateForIndustry
        let cycle = trendWithCycle.cycle
        industry |> Storage.saveIndustryCycle SMA20 cycle |> ignore

    )|> ignore
    
| false ->
    ()

match runCountriesJob() with
| true ->
    let date = DateTime.Parse("2023-10-27")
    TimeFunctions.nowFunc <- fun() -> date
    StockScreenerReports.Web.Services.countriesRun (DummyLogger())
| false -> ()

match runTestReports() with
| true ->
    
    let outputBreakdown (breakdown:IndustrySMABreakdown) =
        Console.WriteLine(breakdown.industry + " " + breakdown.breakdown.percentAbove.ToString())
        
    let latestDate = Reports.getIndustrySMABreakdownLatestDate() |> Utils.convertToDateString
    
    let industryBreakdownsSMA20 = Reports.getIndustrySMABreakdowns SMA.SMA20 latestDate
    let industryBreakdownsSMA200 = Reports.getIndustrySMABreakdowns SMA.SMA200 latestDate
    
    Console.WriteLine("Industries sorted by 20 SMA:")
    industryBreakdownsSMA20
    |> Seq.sortByDescending _.breakdown.percentAbove
    |> Seq.truncate 5
    |> Seq.iter outputBreakdown
    
    let sma20Ranks = industryBreakdownsSMA20 |> Seq.sortByDescending _.breakdown.percentAbove |> Seq.mapi (fun i breakdown -> breakdown.industry, i + 1 |> decimal) |> Map.ofSeq
    let sma200Ranks = industryBreakdownsSMA200 |> Seq.sortByDescending _.breakdown.percentAbove |> Seq.mapi (fun i breakdown -> breakdown.industry, i + 1 |> decimal) |> Map.ofSeq

    let averageRanks = industryBreakdownsSMA20
                       |> Seq.map (fun breakdown -> (breakdown, (sma20Ranks[breakdown.industry] + sma200Ranks[breakdown.industry]) / 2m))
                       |> Seq.sortBy snd
                       
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by average rank:")
    averageRanks
    |> Seq.truncate 5
    |> Seq.iter (fun (breakdown, rank) -> Console.WriteLine(breakdown.industry + " " + rank.ToString()))
    
    // sort by weighted average
    let weightedAverageRanks =
           industryBreakdownsSMA20
           |> Seq.map (fun breakdown -> (breakdown, (sma20Ranks[breakdown.industry] * 0.65m + sma200Ranks[breakdown.industry] * 0.45m)))
           |> Seq.sortBy snd
                       
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by weighted average rank:")
    weightedAverageRanks
    |> Seq.truncate 5
    |> Seq.iter (fun (breakdown, rank) -> Console.WriteLine(breakdown.industry + " " + rank.ToString()))

| false -> ()