module StockScreenerReports.Console.IndustryCorrelationPlayground

open MathNet.Numerics.Statistics
open StockScreenerReports.Core
open StockScreenerReports.Storage

let run () =
    
    // load all industries
    
    let industries = Storage.getIndustries()
    
    // for each industry, load last 90 days of sma breakdowns
    
    let industrySMABreakdowns =
        industries
        |> List.map (fun industry ->
            let breakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (ReportsConfig.dateRangeAsStrings()) industry
            breakdowns |> List.map (fun breakdown -> breakdown.breakdown.percentAbove |> float)
        )
        |> List.map List.toArray
        |> List.toArray
        
    // for each industry, calculate correlation with all other industries
    let correlationMatrix = Correlation.PearsonMatrix(industrySMABreakdowns)
    
    // get top five correlations for each industry one by one
    for i in 0..industries.Length-1 do
        let industry = industries.[i]
        let correlations = correlationMatrix.Row(i)
        let topFiveCorrelations = 
            correlations.ToArray()
            |> Array.mapi (fun j correlation -> (industries[j], correlation))
            |> Array.sortBy snd
            |> Array.take 6
            |> Array.skip 1
        printfn $"Top five correlations for %s{industry}"
        for (industry, correlation) in topFiveCorrelations do
            printfn $"%s{industry}: %f{correlation}"
        printfn ""
        System.Console.ReadLine() |> ignore
    
    // print correlation matrix
    printfn "%A" correlationMatrix