open System
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

match runCountriesJob() with
| true ->
    let date = DateTime.Parse("2023-10-27")
    TimeFunctions.nowFunc <- fun() -> date
    StockScreenerReports.Web.Services.countriesRun (DummyLogger())
| false -> ()


let linearRegression numberOfDays (smaBreakdowns:SMABreakdown seq) =
        
    let x = smaBreakdowns |> Seq.truncate numberOfDays |> Seq.mapi (fun i breakdown -> i, breakdown.percentAbove) |> Seq.map (fun (i, y) -> float i, float y) |> Seq.toArray
    let y = x |> Array.map snd
    let x = x |> Array.map fst
    
    MathNet.Numerics.Fit.line x y |> _.ToTuple()

let detectTrendByMomentum (breakdowns:SMABreakdown seq) numDays lookbackPeriod =
    let percentagesAboveSMA =
        breakdowns
        |> Seq.map (_.percentAbove)
        |> Seq.toList

    let rocValues =
        percentagesAboveSMA
        |> Seq.windowed (lookbackPeriod + 1)
        |> Seq.map (fun window ->
            let currentValue = window[lookbackPeriod]
            let previousValue = window[0]
            let previousValue = if previousValue = 0m then 0.01m else previousValue
           
            (currentValue - previousValue) / previousValue * 100m)
        |> Seq.toList

    if List.length rocValues < numDays then
        None
    else
        let recentROC = List.take numDays rocValues
        let averageROC = List.average recentROC

        Some averageROC
        
match runTestReports() with
| true ->
    
    let outputBreakdown (breakdown:IndustrySMABreakdown) =
        Console.WriteLine(breakdown.industry + " " + breakdown.breakdown.percentAbove.ToString())
        
    let truncateAndOutput outputFunc (seq:seq<'a>) =
        seq |> Seq.truncate 10 |> Seq.iter outputFunc
        // and then the last 10
        Console.WriteLine("...")
        seq |> Seq.skip (Seq.length seq - 10) |> Seq.iter outputFunc
        
    let latestDate = Reports.getIndustrySMABreakdownLatestDate()
    let latestDateStr = latestDate |> Utils.convertToDateString
    
    let industryBreakdownsSMA20 = Reports.getIndustrySMABreakdowns SMA.SMA20 latestDateStr
    let industryBreakdownsSMA200 = Reports.getIndustrySMABreakdowns SMA.SMA200 latestDateStr
    
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
    averageRanks |> truncateAndOutput (fun (breakdown, rank) -> Console.WriteLine(breakdown.industry + " " + rank.ToString()))
    
    // sort by weighted average
    let weightedAverageRanks =
           industryBreakdownsSMA20
           |> Seq.map (fun breakdown -> (breakdown, (sma20Ranks[breakdown.industry] * 0.65m + sma200Ranks[breakdown.industry] * 0.45m)))
           |> Seq.sortBy snd
                       
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by weighted average rank:")
    weightedAverageRanks |> truncateAndOutput (fun (breakdown, rank) -> Console.WriteLine(breakdown.industry + " " + rank.ToString()))
    
    // sort by trend
    let trendRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry
            (breakdown, TrendsCalculator.calculateSMACrossOverStrength smaBreakdowns))
        |> Seq.sortByDescending snd
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by sma cross over strength:")
    trendRanks |> truncateAndOutput (fun (breakdown, strength) -> Console.WriteLine(breakdown.industry + " " + strength.ToString()))
    
    // sort by ema cross over strength
    let emaTrendRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry
            (breakdown, TrendsCalculator.calculateEMACrossOverStrength smaBreakdowns))
        |> Seq.sortByDescending snd
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by ema cross over strength:")
    emaTrendRanks |> truncateAndOutput (fun (breakdown, strength) -> Console.WriteLine(breakdown.industry + " " + strength.ToString()))
    
    // sort by linear regression method
    let linearRegressionRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry |> Seq.map _.breakdown
            (breakdown, linearRegression 20 smaBreakdowns))
        |> Seq.sortByDescending (fun (_, (_, slope)) -> slope)
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by linear regression slope:")
    linearRegressionRanks |> truncateAndOutput (fun (breakdown, (intercept, slope)) -> Console.WriteLine(breakdown.industry + " " + slope.ToString() + " " + intercept.ToString()))
    
    
    // sort by momentum
    let momentumRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry |> Seq.map _.breakdown
            (breakdown, detectTrendByMomentum smaBreakdowns 10 10))
        |> Seq.sortByDescending (fun (_, strength) -> strength)
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by momentum:")
    momentumRanks |> truncateAndOutput (fun (breakdown, strength) -> Console.WriteLine(breakdown.industry + " " + strength.ToString()))
    
    
    // sort by adx
    let adxRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry
            (breakdown, TrendsCalculator.calculateADXTrend smaBreakdowns))
        |> Seq.sortByDescending snd
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by adx:")
    adxRanks |> truncateAndOutput (fun (breakdown, strength) -> Console.WriteLine(breakdown.industry + " " + strength.ToString()))

| false -> ()