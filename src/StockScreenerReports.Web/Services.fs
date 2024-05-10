namespace StockScreenerReports.Web

module Services =
    open StockScreenerReports.Storage
    open Microsoft.Extensions.Logging
    open StockScreenerReports.Core
    open StockScreenerReports.FinvizClient
    open System
    open System.Collections.Generic

    let runIfTradingDay func =
        let isTradingDay = ReportsConfig.now().Date |> ReportsConfig.isTradingDay
        match isTradingDay with
        | true -> func()
        | false -> ()

    let screenerRun (logger:ILogger) =
        let fetchScreenerResults (input:Screener) =
            logger.LogInformation $"Running screener %s{input.name}"
            let results = FinvizClient.getResults input.url
            (input,results)

        let saveToDb (screenerResults:list<Screener * 'a>) =
            logger.LogInformation("Saving results to db")
            let date = Utils.getRunDate()
            screenerResults
            |> List.iter (fun x -> Storage.saveScreenerResults date x)
            screenerResults

        let funcToRun() =
            let results =
                Storage.getScreeners()
                |> List.map fetchScreenerResults
                |> saveToDb

            let emptyResults = results |> List.filter (fun (_,results) -> results.Length = 0)
            
            let status, message =
                match emptyResults with
                | [] ->
                    (
                        Success,
                        $"Ran {results.Length} screeners successfully"
                    )
                | _ ->
                    let screenerNamesWithNoResults = emptyResults |> List.map (fun (screener,_) -> screener.name) |> String.concat ", "
                    
                    (
                        Warning,
                        $"Ran {results.Length} screeners successfully, but {screenerNamesWithNoResults} screener(s) had no results"
                    )
            
            Storage.saveJobStatus ScreenerJob (ReportsConfig.nowUtcNow()) status message |> ignore

        runIfTradingDay funcToRun

    let earningsRun (logger:ILogger) =
        
        let funcToRun() =
            let earnings = FinvizClient.getEarnings()

            let message = $"Ran {earnings.Length} earnings successfully"

            logger.LogInformation(message)

            earnings
                |> List.iter (fun x ->
                    let ticker,earningsTime = x
                    Storage.saveEarningsDate ticker (Utils.getRunDate()) earningsTime |> ignore
                )

            Storage.saveJobStatus EarningsJob (ReportsConfig.nowUtcNow()) Success message |> ignore

        runIfTradingDay funcToRun
        
    let corporateActionsRun (_:ILogger) =
        let funcToRun() =
            try
                let actions = StockAnalysisClient.getCorporateActions()
                
                match actions with
                | [] ->
                    // fail if no actions were fetched, something is off
                    let errorMsg = "No corporate actions found."
                    Storage.saveJobStatus CorporateActionsJob (ReportsConfig.nowUtcNow()) Failure errorMsg |> ignore
                | _ ->
                    let saved = Storage.saveCorporateActions actions
                    Storage.saveJobStatus CorporateActionsJob (ReportsConfig.nowUtcNow()) Success $"{saved} corporate actions fetched and saved successfully." |> ignore
                
                // go over each action, and if today is the action date, create alert for it
                actions
                |> List.iter (fun action ->
                    let actionDate = action.Date |> DateTime.Parse
                    let today = ReportsConfig.now().Date
                    if actionDate = today then
                        // see if we have a stock for it
                        let stock = action.Symbol |> StockTicker.create |> Storage.getStockByTicker
                        match stock with
                        | Some stock ->
                            let alert = { date = actionDate
                                          sentiment = Neutral
                                          description = $"Corporate action for {action.Symbol} - {stock.company}, {action.Type}: {action.Action}"
                                          strength = 0m
                                          alertType = CorporateActionAlert(action.Symbol, action.Type)
                                          acknowledged = false }
                            Storage.saveAlert alert |> ignore
                        | None ->
                            ()
                )
            with
            | ex ->
                let errorMsg = $"Error fetching and saving corporate actions: {ex.Message}"
                Storage.saveJobStatus CorporateActionsJob (ReportsConfig.nowUtcNow()) Failure errorMsg |> ignore
                reraise()
            
        runIfTradingDay funcToRun
    
    let alertsRun (logger:ILogger) =
        
        let funcToRun() =
            
            let screeners = Storage.getScreeners()
            
            let industries = Storage.getIndustries()
            
            let dateRange = ReportsConfig.dateRangeAsStrings()
                
            let screenerDate = dateRange |> snd |> Reports.getScreenerResultsLastKnownDateAsOf |> Utils.convertToDateString
            
            // load latest screener hits for each screener
            let screenersWithResults =
                screeners
                |> List.map (fun s ->
                    s, screenerDate |> Reports.getScreenerResults s.id
                )
                
            let industrySize = industries |> List.map (fun industry ->
                let breakdowns = industry |> Reports.getIndustrySMABreakdownsForDateRange SMA20 dateRange 
                industry, breakdowns[breakdowns.Length - 1].breakdown.total) |> Map.ofList 
            
            let screenerAlerts = 
                IndustryAlertGenerator.screenerAlerts industrySize screenersWithResults
                |> List.map Storage.saveAlert
                |> List.sum
            
            let industriesWithSequences =
                industries
                |> List.map (fun industry ->
                    industry, industry |> Storage.getIndustrySequencesForIndustry)
                |> Map.ofList
            
            // let's load industry sequences and see if there are any recent ones
            let sequenceAlerts =
                IndustryAlertGenerator.industrySequenceAlerts (ReportsConfig.now()) industriesWithSequences
                |> List.map Storage.saveAlert
                |> List.sum
                
            let message = $"Generated {screenerAlerts} screener alerts and {sequenceAlerts} sequence alerts"
            
            logger.LogInformation(message)
            
            Storage.saveJobStatus AlertsJob (ReportsConfig.nowUtcNow()) Success message |> ignore
            
        runIfTradingDay funcToRun

    let countriesRun (logger:ILogger) =
        
        let funcToRun () =
            
            let date = Utils.getRunDate()
            
            logger.LogInformation($"Running countries for {date}")
            
            let countries =
                Storage.getCountries()
                |> List.filter ReportsConfig.includeCountryInScans
            
            let countrySmaPairs = countries |> List.map (fun country -> SMA.All |> List.map (fun sma -> (country, sma))) |> Seq.concat
            
            let countriesUpdated =
                countrySmaPairs
                |> Seq.map (fun (country,sma) ->
                    logger.LogInformation($"Processing country {country} {sma} day sma breakdown")
                    (country,sma))
                |> Seq.map (fun (country, sma) ->   
                    let above,below = country |> FinvizClient.getResultCountForCountryAboveAndBelowSMA sma
                    (country, sma, above, below)
                )
                |> Seq.map (fun r ->
                    Storage.saveCountrySMABreakdowns date r |> ignore
                )
                |> Seq.length
                
            Storage.saveJobStatus
                CountriesJob
                (ReportsConfig.nowUtcNow())
                Success
                $"Updated sma breakdowns for {countriesUpdated} countries"
            |> ignore
            
        try
            runIfTradingDay funcToRun
        with
        | ex -> 
            let message = $"Error running countries: {ex.Message}"
            logger.LogError(ex, message)
            Storage.saveJobStatus CountriesJob (ReportsConfig.nowUtcNow()) Failure message |> ignore
            
    let trendsRun (logger:ILogger) =
        let funcToRun() =
            let date = Utils.getRunDate()

            logger.LogInformation($"Running trends for {date}")
        
            // pull above and below 20 and 200 for each industry, and store the results
            let knownIndustries = Storage.getIndustries()
            
            let industrySmaPairs = knownIndustries |> Seq.map (fun industry -> SMA.All |> List.map (fun sma -> (industry, sma))) |> Seq.concat

            let industriesUpdated =
                industrySmaPairs
                |> Seq.map (fun (industry,sma) ->
                    logger.LogInformation($"Processing industry {industry} {sma} day sma breakdown")
                    (industry,sma))
                |> Seq.map (fun (industry, sma) ->   
                    let above,below = industry |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA sma
                    (industry, sma, above, below)
                )
                |> Seq.map (fun r ->
                    Storage.saveIndustrySMABreakdowns date r |> ignore
                )
                |> Seq.length

            SMA.All |> List.iter (fun sma -> Storage.updateIndustrySMABreakdowns date sma |> ignore)

            logger.LogInformation($"Calculating trends")

            let trendsUpdated =
                industrySmaPairs
                |> Seq.map (fun (industry, days) -> 
                    
                    let breakdowns = industry |> Reports.getIndustrySMABreakdownsForDateRange days (ReportsConfig.dateRangeAsStrings())
                    let trendAndCycle = breakdowns |> TrendsCalculator.calculateForIndustry
                    let trend = trendAndCycle.trend
                    let lastBreakdown = breakdowns |> List.last
                    logger.LogInformation($"Saving industry {industry} trend: {trend.direction} {trend.streak} days with change of {trend.change}")
                    Storage.updateIndustryTrend lastBreakdown trend |> ignore
                    
                    // store sequences but only if days is SMA20
                    if days = SMA.SMA20 then
                        let sequences = breakdowns |> TrendsCalculator.calculateSequences
                        logger.LogInformation($"Saving industry {industry} sequences")
                        sequences |> List.iter Storage.saveIndustrySequenceWithPoints
                    
                    let cycle = trendAndCycle.cycle
                    Storage.saveIndustryCycle days cycle industry
                    
                ) |> Seq.sum

            Storage.saveJobStatus
                TrendsJob
                (ReportsConfig.nowUtcNow())
                Success
                $"Updated sma breakdowns for {industriesUpdated} industries and calculated {trendsUpdated} trends"
            |> ignore

        try
            runIfTradingDay funcToRun
        with
        | ex -> 
            let message = $"Error running trends: {ex.Message}"
            logger.LogError(ex, message)
            Storage.saveJobStatus TrendsJob (ReportsConfig.nowUtcNow()) Failure message |> ignore

    // background service class
    type BackgroundService(logger:ILogger<BackgroundService>) =
        inherit Microsoft.Extensions.Hosting.BackgroundService()

        let runTracker = HashSet<string>()

        let doSleepInternal (sleepTime:int) =
            logger.LogInformation($"Sleeping for {sleepTime} milliseconds")
            System.Threading.Tasks.Task.Delay(sleepTime)
            
        let doSleep() =
            60 * 60 * 1000 |> doSleepInternal

        let shortSleep() =
            10 * 1000 |> doSleepInternal

        override _.ExecuteAsync(cancellationToken) =
            task {
                while cancellationToken.IsCancellationRequested |> not do
                    
                        
                logger.LogInformation("---- background service")
                let now = ReportsConfig.now()
                let time = now.TimeOfDay
                let marketClose = TimeSpan(16,0,0)
                let marketCloseDelayed = marketClose.Add(TimeSpan.FromMinutes(30))
                if runTracker.Count = 0 then
                    logger.LogInformation("Run tracker is empty, warming up")
                    runTracker.Add("warmup") |> ignore
                    let! _ = shortSleep()
                    logger.LogInformation("Short sleep finished")
                else if time < marketCloseDelayed then
                    logger.LogInformation("Not past market close time")
                    let! _ = doSleep()
                    logger.LogInformation("Finished sleeping")
                else
                    try
                        logger.LogInformation("Past market close, checking if already ran today")
                        let runDate = Utils.getRunDate()
                        if runTracker.Contains(runDate) then
                            logger.LogInformation("Already ran today")
                        else
                            logger.LogInformation("Running")
                            runTracker.Add(runDate) |> ignore
                            screenerRun logger
                            earningsRun logger
                            trendsRun logger
                            countriesRun logger
                            alertsRun logger
                            corporateActionsRun logger
                            logger.LogInformation("Finished running")
                    with
                    | ex -> logger.LogError(ex, "Error running background service")

                    let! _ = doSleep()

                    logger.LogInformation("Finished sleeping")
            }