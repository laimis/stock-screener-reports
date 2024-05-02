open System
open StockScreenerReports.Console
open StockScreenerReports.Core
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

let runSequencesMigration() =
    containsArgument "--sequences-migration"

let runCyclesMigration() =
    containsArgument "--cycles-migration"
    
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
    let logger = DummyLogger()
    StockScreenerReports.Web.Services.screenerRun logger
    StockScreenerReports.Web.Services.earningsRun logger
| false -> ()


match runSMAUpdates() with
| true ->     
    StockScreenerReports.Web.Services.trendsRun (DummyLogger())
| false -> ()

match runSequencesMigration() with
| true ->
    let knownIndustries = Storage.getIndustries()

    knownIndustries
    |> Seq.iter (fun industry -> 
        
        industry
        |> Reports.getAllIndustrySMABreakdowns SMA20
        |> TrendsCalculator.calculateSequences
        |> List.iter Storage.saveIndustrySequenceWithPoints
    )
    
| false ->
    ()

match runCyclesMigration() with
| true ->
    
    let industries = Storage.getIndustries()
    
    let industrySMAPairs = SMA.All |> Seq.map (fun sma -> industries |> Seq.map (fun industry -> (industry, sma))) |> Seq.concat
    
    let startDate = DateTime.Parse("2023-01-01")
    let random = new Random()
    
    industrySMAPairs
    |> Seq.iter (fun pairs -> 
        let industry, sma = pairs
        
        Seq.initInfinite id
        |> Seq.map startDate.AddDays
        |> Seq.takeWhile (fun i -> i < now)
        |> Seq.filter (fun i -> i |> ReportsConfig.isTradingDay)
        |> Seq.iter(fun date ->
            
            // every 1% of checks, print what sma, industry, date is being processed
            if random.Next(100) = 0 then
                Console.WriteLine("Processing " + industry + " " + sma.ToString() + " " + date.ToString())
                
            let range = (date.AddDays(-ReportsConfig.days), date) |> ReportsConfig.formatDateRangeToStrings
            
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForDateRange sma range industry
            
            if smaBreakdowns.Length = 0 then
                Console.WriteLine("No data found for " + industry + " " + sma.ToString() + " " + date.ToString())
            else
                let trendWithCycle = smaBreakdowns |> TrendsCalculator.calculateForIndustry
                Storage.saveIndustryCycle sma trendWithCycle.cycle industry |> ignore
        )
    )
    
| false -> ()
        
match runTestReports() with
| true ->
    
    // IndustryRankingPlayground.run()
    IndustryCorrelationPlayground.run()

| false -> ()