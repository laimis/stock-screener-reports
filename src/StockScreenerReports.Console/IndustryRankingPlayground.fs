module StockScreenerReports.Console.IndustryRankingPlayground

open System
open StockScreenerReports.Core
open StockScreenerReports.Storage

let roundedDecimal (x:decimal) =
    Math.Round(x, 2)
    
let roundedFloat (x:float) =
    Math.Round(x, 2)

let linearRegression numberOfDays (smaBreakdowns:SMABreakdown list) =
    
    let data = smaBreakdowns[(smaBreakdowns.Length - numberOfDays)..] |> Seq.mapi (fun i breakdown -> i |> float, breakdown.percentAbove |> float) |> Seq.toArray
    let y = data |> Array.map snd
    let x = data |> Array.map fst
    MathNet.Numerics.Fit.line x y |> _.ToTuple() |> fun (intercept, slope) -> (roundedFloat intercept, roundedFloat slope)
    
let outputBreakdown (breakdown:IndustrySMABreakdown) =
        Console.WriteLine(breakdown.industry + " " + breakdown.breakdown.percentAbove.ToString())
        
let truncateAndOutput outputFunc (seq:seq<'a>) =
    seq |> Seq.truncate 10 |> Seq.iter outputFunc
    // and then the last 10
    Console.WriteLine("...")
    seq |> Seq.skip (Seq.length seq - 10) |> Seq.iter outputFunc
        
    
let run () =
    let latestDate = Reports.getIndustrySMABreakdownLatestDate()
    let latestDateStr = latestDate |> Utils.convertToDateString
    
    let industryBreakdownsSMA20 = Reports.getIndustrySMABreakdownsForDate SMA.SMA20 latestDateStr
    let industryBreakdownsSMA200 = Reports.getIndustrySMABreakdownsForDate SMA.SMA200 latestDateStr
    
    let getSMA20Breakdown industry = industryBreakdownsSMA20 |> Seq.find (fun b -> b.industry = industry)
    let getSMA200Breakdown industry = industryBreakdownsSMA200 |> Seq.find (fun b -> b.industry = industry)
    
    Console.WriteLine("Industries sorted by 20 SMA:")
    industryBreakdownsSMA20
    |> Seq.sortByDescending _.breakdown.percentAbove
    |> Seq.truncate 5
    |> Seq.iter outputBreakdown
    
    let averageRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            let breakdown20 = breakdown.industry |> getSMA20Breakdown
            let breakdown200 = breakdown.industry |> getSMA200Breakdown
            let score = TrendsCalculator.calculateAverageOfSMA20AndSMA200 breakdown20 breakdown200
            (breakdown, score)
        )
        |> Seq.sortByDescending snd
                       
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by average rank:")
    averageRanks |> truncateAndOutput (fun (breakdown, rank) -> Console.WriteLine(breakdown.industry + " " + rank.ToString()))
    
    // sort by weighted average
    let weightedAverageRanks =
           industryBreakdownsSMA20
           |> Seq.map (fun breakdown ->
               let breakdown20 = breakdown.industry |> getSMA20Breakdown
               let breakdown200 = breakdown.industry |> getSMA200Breakdown
               let score = TrendsCalculator.calculateWeightedRankOfSMA20AndSMA200 breakdown20 breakdown200
               (breakdown, score))
           |> Seq.sortByDescending snd
                       
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by weighted average rank:")
    weightedAverageRanks |> truncateAndOutput (fun (breakdown, rank) -> Console.WriteLine(breakdown.industry + " " + rank.ToString()))
    
    // sort by trend
    let trendRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry
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
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry
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
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry |> List.map _.breakdown
            (breakdown, linearRegression 20 smaBreakdowns))
        |> Seq.sortByDescending (fun (_, (_, slope)) -> slope)
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by linear regression slope:")
    linearRegressionRanks |> truncateAndOutput (fun (breakdown, (intercept, slope)) -> Console.WriteLine($"{breakdown.industry}: {intercept} + {slope}x"))
    
    
    // sort by adx
    let adxRanks =
        industryBreakdownsSMA20
        |> Seq.map (fun breakdown ->
            // get the last 90 days
            let numDays = 90
            let smaBreakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (latestDate.AddDays(-numDays) |> Utils.convertToDateString, latestDateStr) breakdown.industry
            (breakdown, TrendsCalculator.calculateADXTrend smaBreakdowns))
        |> Seq.sortByDescending snd
        
    Console.WriteLine("")
    Console.WriteLine("Industries sorted by adx:")
    adxRanks |> truncateAndOutput (fun (breakdown, strength) -> Console.WriteLine(breakdown.industry + " " + strength.ToString()))