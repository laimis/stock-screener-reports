module StockScreenerReports.Core.IndustryAlertGenerator

[<Literal>]
let private RESULT_COUNT_THRESHOLD = 20
[<Literal>]
let private PERCENT_CHECK_THRESHOLD = 0.5
[<Literal>]
let private INDUSTRY_PERCENT_CHECK_THRESHOLD = 0.4
[<Literal>]
let private RESULT_MINIMUM_THRESHOLD = 5

let screenerAlerts industrySizeMap screenersWithResults =
    
    let countCheck industryName screener (results:ScreenerResultReportItem list) =
        
        match results |> List.length with
        | x when x >= RESULT_COUNT_THRESHOLD ->
            let alert = {
                industry = industryName
                alertType = "Count"
                description = industryName + " has large participation, " + x.ToString() + " results"
                screener = screener
                date = results.Head.date
                sentiment = Positive
            }
            Some alert
        | _ -> None
        
    let percentCheck industryName screener (industryResults:list<ScreenerResultReportItem>) totalResults =
        
        match (float industryResults.Length) / (float totalResults) with
        | x when x >= PERCENT_CHECK_THRESHOLD ->
            let alert = {
                industry = industryName
                alertType = "Percent"
                description = $"{industryName} has " + x.ToString("P2") + $" percent of the results {industryResults.Length}/{totalResults}"
                screener = screener
                date = industryResults.Head.date
                sentiment = Positive
            }
            Some alert
        | _ -> None
        
    let industryPercentCheck industryName screener (industryResults:ScreenerResultReportItem list) =
        
        let industryTotal = industrySizeMap |> Map.find industryName
        
        match (float industryResults.Length) / (float industryTotal) with
        | x when x >= INDUSTRY_PERCENT_CHECK_THRESHOLD ->
            let alert = {
                industry = industryName
                alertType = "IndustryPercent"
                description = $"{industryName} has " + x.ToString("P2") + $" of the industry participants {industryResults.Length}/{industryTotal}"
                screener = screener
                date = industryResults.Head.date
                sentiment = Positive 
            }
            Some alert
        | _ -> None
    
    // we will go over each screener,results pair in the screenersWithResults
    // and generate an alert for each industry that has a result
    screenersWithResults
    |> List.map (fun (screener:Screener, results:ScreenerResultReportItem list) ->
        
        match results.Length with
        | x when x < RESULT_MINIMUM_THRESHOLD -> []
        | _ ->
            results
            |> List.groupBy (fun (r:ScreenerResultReportItem) -> r.industry)
            |> List.map (fun (industry, industryResults) ->
                [
                    countCheck industry screener industryResults
                    percentCheck industry screener industryResults results.Length
                    industryPercentCheck industry screener industryResults
                ]
            )
            |> List.concat
            |> List.choose id
    )
    |> List.concat
    
    
[<Literal>]
let private SMA_ALERT_HIGH_PERCENT_THRESHOLD = 90

[<Literal>]
let private SMA_ALERT_LOW_PERCENT_THRESHOLD = 20

let industryTrendAlerts (industryTrends:IndustryTrend list) =
    
    industryTrends
    |> List.filter (fun (industryTrend:IndustryTrend) -> industryTrend.percentAbove >= decimal SMA_ALERT_HIGH_PERCENT_THRESHOLD)
    |> List.map (fun (industryTrend:IndustryTrend) ->
        {
            industry = industryTrend.industry
            alertType = "SMA"
            description = $"{industryTrend.industry} has {industryTrend.percentAboveFormatted} of stocks above {industryTrend.days.Interval} day SMA"
            date = industryTrend.date
            sentiment = Positive 
        }
    )
    
    |> List.append
        (industryTrends
        |> List.filter (fun (industryTrend:IndustryTrend) -> industryTrend.percentAbove <= decimal SMA_ALERT_LOW_PERCENT_THRESHOLD)
        |> List.map (fun (industryTrend:IndustryTrend) ->
            {
                industry = industryTrend.industry
                alertType = "SMA"
                description = $"{industryTrend.industry} has {industryTrend.percentAboveFormatted} of stocks below {industryTrend.days.Interval} day SMA"
                date = industryTrend.date
                sentiment = Negative 
            }
        )
    )