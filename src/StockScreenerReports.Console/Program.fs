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

let overrideDate = containsArgument "--override-date"
if overrideDate then
    let date = DateTime.Now.AddDays(-1)
    TimeFunctions.nowFunc <- fun() -> date

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

let fetchScreenerResults input =
    Console.WriteLine("Processing " + input.name)
    Console.WriteLine("fetching results...")
    let results = FinvizClient.getResults input.url
    (input,results)

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
        let breakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry 20 range
        let trendWithCycle = breakdowns |> TrendsCalculator.calculateTrendAndCycleForIndustry
        let cycle = trendWithCycle.cycle
        industry |> Storage.saveIndustryCycle 20 cycle |> ignore

    )|> ignore
    
| false ->
    ()


match runTestReports() with
| true -> 

    let industry = "Lumber & Wood Production"

    Console.WriteLine("Running calculations for " + industry)

    let smaBreakdowns = 
        industry
        |> Reports.getIndustrySMABreakdownsForIndustry 20 (ReportsConfig.dateRangeAsStrings())

    smaBreakdowns
    |> List.iter (fun x -> Console.Write($"({x.breakdown.above}, {x.breakdown.above + x.breakdown.below});"))
    Console.WriteLine("")

    let trend = 
        smaBreakdowns
        |> TrendsCalculator.calculateForIndustry

    Console.WriteLine($"{industry} {20} days sma streak: {trend.streak} day {trend.direction} with change of {trend.change}")

| false -> ()