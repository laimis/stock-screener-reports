module StockScreenerReports.Core.IndustryAlertGenerator

[<Literal>]
let private RESULT_COUNT_THRESHOLD = 20

let private PERCENT_CHECK_THRESHOLD = 0.5m
let private INDUSTRY_PERCENT_CHECK_THRESHOLD = 0.4m

[<Literal>]
let private RESULT_MINIMUM_THRESHOLD = 5

let screenerAlerts industrySizeMap screenersWithResults =
    
    let countCheck industryName screener (results:ScreenerResultReportItem list) =
        
        match results |> List.length with
        | x when x >= RESULT_COUNT_THRESHOLD ->
            let alert = {
                acknowledged = false
                description = industryName + " has large participation, " + x.ToString() + " results"
                date = results.Head.date
                sentiment = Positive
                strength = x |> decimal
                alertType = IndustryScreenerAlert(industryName,screener.id)
            }
            Some alert
        | _ -> None
        
    let percentCheck industryName screener (industryResults:list<ScreenerResultReportItem>) totalResults =
        
        match (decimal industryResults.Length) / (decimal totalResults) with
        | x when x >= PERCENT_CHECK_THRESHOLD ->
            let alert = {
                acknowledged = false
                description = $"{industryName} has " + x.ToString("P2") + $" percent of the results {industryResults.Length}/{totalResults}"
                date = industryResults.Head.date
                sentiment = Positive
                strength = x
                alertType = IndustryScreenerAlert(industryName,screener.id)
            }
            Some alert
        | _ -> None
        
    let industryPercentCheck industryName screener (industryResults:ScreenerResultReportItem list) =
        
        let industryTotal = industrySizeMap |> Map.find industryName
        
        match (decimal industryResults.Length) / (decimal industryTotal) with
        | x when x >= INDUSTRY_PERCENT_CHECK_THRESHOLD ->
            let alert = {
                acknowledged = false
                description = $"{industryName} has " + x.ToString("P2") + $" of the industry participants {industryResults.Length}/{industryTotal}"
                date = industryResults.Head.date
                sentiment = Positive
                strength = x
                alertType = IndustryScreenerAlert(industryName,screener.id)
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

let industrySequenceAlerts (referenceDate:System.DateTime) (industryTrends:Map<string, IndustrySequence list>) =
    
    let startAlerts =
        industryTrends
        |> Map.toList
        |> List.map(fun (_,sequences) ->
            sequences
            |> List.filter(fun (sequence:IndustrySequence) ->
                let today = referenceDate.Date
                sequence.start.date = today 
            )
            |> List.tryHead
        )
        |> List.choose id
        |> List.map( fun (sequence:IndustrySequence) ->
            {
                date = referenceDate.Date
                alertType = IndustryAlert(sequence.industry)
                acknowledged = false
                description = $"{sequence.industry} has entered {sequence.type'} sequence"
                sentiment = match sequence.type' with | High -> Positive | Low -> Negative
                strength = sequence.end'.value
            }
        )
        
    let endAlerts =
        industryTrends
        |> Map.toList
        |> List.map(fun (_,sequences) ->
            sequences
            |> List.filter(fun (sequence:IndustrySequence) ->
                let today = referenceDate.Date
                today.Subtract(sequence.end'.date).TotalDays |> int <= 1 && sequence.open' = false
            )
            |> List.tryHead
        )
        |> List.choose id
        |> List.map( fun (sequence:IndustrySequence) ->
            {
                date = referenceDate.Date
                alertType = IndustryAlert(sequence.industry)
                acknowledged = false
                description = $"{sequence.industry} has exited {sequence.type'} sequence"
                sentiment = match sequence.type' with | High -> Negative | Low -> Positive
                strength = sequence.end'.value
            }
        )
        
    startAlerts @ endAlerts
    