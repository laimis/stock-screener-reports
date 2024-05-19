namespace StockScreenerReports.Web

module Services =
    open StockScreenerReports.Storage
    open Microsoft.Extensions.Logging
    open StockScreenerReports.Core
    open StockScreenerReports.FinvizClient
    open System
    open System.Collections.Generic

    let runIfTradingDay forced func = task {
        let isTradingDay = ReportsConfig.now().Date |> ReportsConfig.isTradingDay
        
        match isTradingDay || forced with
        | true -> return! func()
        | false -> return Skipped
    }

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

        let funcToRun() = task {
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
            
            return status
        }
        
        runIfTradingDay false funcToRun

    let earningsRun (logger:ILogger) =
        
        let funcToRun() = task {
            let earnings = FinvizClient.getEarnings()

            let message = $"Ran {earnings.Length} earnings successfully"

            logger.LogInformation(message)

            earnings
                |> List.iter (fun x ->
                    let ticker,earningsTime = x
                    Storage.saveEarningsDate ticker (Utils.getRunDate()) earningsTime |> ignore
                )

            let jobStatus = Success
            
            Storage.saveJobStatus EarningsJob (ReportsConfig.nowUtcNow()) jobStatus message |> ignore
            
            return jobStatus
        }
        
        runIfTradingDay false funcToRun
        
    let corporateActionsRun forced (_:ILogger) =
        let funcToRun() = task {
            try
                let actions = StockAnalysisClient.getCorporateActions()
                
                match actions with
                | [] ->
                    // fail if no actions were fetched, something is off
                    let errorMsg = "No corporate actions found."
                    let status = Failure
                    Storage.saveJobStatus CorporateActionsJob (ReportsConfig.nowUtcNow()) status errorMsg |> ignore
                    return status
                | _ ->
                    let saved = Storage.saveCorporateActions actions
                    let status = Success
                    Storage.saveJobStatus CorporateActionsJob (ReportsConfig.nowUtcNow()) status $"{saved} corporate actions fetched and saved successfully." |> ignore
                    return status
                    
            with
            | ex ->
                let errorMsg = $"Error fetching and saving corporate actions: {ex.Message}"
                let status = Failure
                Storage.saveJobStatus CorporateActionsJob (ReportsConfig.nowUtcNow()) status errorMsg |> ignore
                return status
        }
        
        runIfTradingDay forced funcToRun
    
    let alertsRun (logger:ILogger) =
        
        let funcToRun() = task {
            
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
                
            // see if there are corporate actions one might take a look at
            let! corporateActions = ReportsConfig.now() |> Storage.getCorporateActionsForDate
                
            let alerts =
                corporateActions
                |> List.map (fun (action:CorporateAction) ->
                    let stockSymbolToLookup =
                        match action.Type with
                        | SymbolChange (oldSymbol, _) -> oldSymbol
                        | _ -> action.Symbol
                        
                    let stock = stockSymbolToLookup |> StockTicker.create |> Storage.getStockByTicker
                    match stock with
                    | Some stock -> Some (stock, action)
                    | None -> None
                )
                |> List.choose id
                |> List.map ( fun (stock, action) ->
                    {
                          date = action.Date
                          sentiment = Neutral
                          description = $"Corporate action for {action.Symbol} - {stock.company}, {action.Type}: {action.Action}"
                          strength = 0m
                          alertType = CorporateActionAlert(action.Symbol)
                          acknowledged = false
                    }
                )
            
            alerts
            |> List.map Storage.saveAlert
            |> ignore
                    
            let message = $"Generated {screenerAlerts} screener alerts, {sequenceAlerts} sequence alerts, and {alerts.Length} corporate action alerts"
            
            logger.LogInformation(message)
            
            let status = Success
            
            Storage.saveJobStatus AlertsJob (ReportsConfig.nowUtcNow()) status message |> ignore
            
            return status
        }
        
        runIfTradingDay false funcToRun

    let countriesRun (logger:ILogger) =
        
        let funcToRun () = task {
            
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
                
            let status = Success
            Storage.saveJobStatus
                CountriesJob
                (ReportsConfig.nowUtcNow())
                status
                $"Updated sma breakdowns for {countriesUpdated} countries"
            |> ignore
            return status
        }
        
        runIfTradingDay false funcToRun
            
    let trendsRun (logger:ILogger) =
        let funcToRun() = task {
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

            let status = Success
            
            Storage.saveJobStatus
                TrendsJob
                (ReportsConfig.nowUtcNow())
                status
                $"Updated sma breakdowns for {industriesUpdated} industries and calculated {trendsUpdated} trends"
            |> ignore
            
            return status
        }
        
        runIfTradingDay false funcToRun

    type CorporateActionProcessor(logger:ILogger<CorporateActionProcessor>) =
        
        member this.BankruptyDelistings() = task {
            
            // actions for the day
            let! actions = Storage.getCorporateActions()
            
            // group it by symbol
            let actionsBySymbol = actions |> List.groupBy (fun a -> a.Symbol,a.Date)
            
            // go over each group, and the group that has Delisted and symbol change, take the old symbol and delete it from our db
            let stocksToDelete =
                actionsBySymbol
                |> List.map (fun (_, actions) ->
                    let delisted = actions |> List.tryFind (fun x -> x.Type = Delisted)
                    let symbolChange = actions |> List.tryFind (fun x -> match x.Type with | SymbolChange _ -> true | _ -> false)
                    let bankrupcy = actions |> List.tryFind (fun x -> x.Type = Bankruptcy)
                    (delisted, symbolChange, bankrupcy)
                )
                |> List.filter (fun (delisted, symbolChange, bankruptcy) -> delisted.IsSome && symbolChange.IsSome && bankruptcy.IsSome)
                |> List.map (fun (_, symbolChange, _) -> match symbolChange.Value.Type with | SymbolChange(oldSymbol, _) -> oldSymbol | _ -> failwith "Invalid symbol change")
                |> List.map (fun x -> x |> StockTicker.create |> Storage.getStockByTicker)
                |> List.choose id
                
            let recordsDeleted =
                stocksToDelete
                |> List.map Storage.deleteStock
                |> List.concat
                |> List.sum

            return stocksToDelete, recordsDeleted
        }
        
        member this.Delistings() = task {
            
            // actions
            let! actions = Storage.getCorporateActions()
            
            // group it by symbol and date
            let actionsBySymbol = actions |> List.groupBy (fun a -> a.Symbol,a.Date)
            
            // go over each group, and the group that has Delisted ONLY, can be used for deletion
            let stocksToDelete =
                actionsBySymbol
                |> List.map (fun (_, actions) ->
                    let delisted = actions |> List.tryFind (fun x -> x.Type = Delisted)
                    delisted, actions.Length
                )
                |> List.filter (fun (delisted, count) -> delisted.IsSome && count = 1)
                |> List.map (fun (delisted, _) -> delisted.Value.Symbol)
                |> List.map (fun x -> x |> StockTicker.create |> Storage.getStockByTicker)
                |> List.choose id
                
            let recordsDeleted =
                stocksToDelete
                |> List.map Storage.deleteStock
                |> List.concat
                |> List.sum

            return stocksToDelete, recordsDeleted
        }
            
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
                            
                            let! _ = screenerRun logger
                            let! _ = earningsRun logger
                            let! _ = trendsRun logger
                            let! _ = countriesRun logger
                            let! _ = corporateActionsRun false logger
                            let! _ = alertsRun logger
                            logger.LogInformation("Finished running")
                    with
                    | ex -> logger.LogError(ex, "Error running background service")

                    let! _ = doSleep()

                    logger.LogInformation("Finished sleeping")
            }