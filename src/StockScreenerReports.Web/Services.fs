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
                        Failure,
                        $"Ran {results.Length} screeners successfully, but {screenerNamesWithNoResults} screeners had no results"
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
                    
                    let breakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry days (ReportsConfig.dateRangeAsStrings())
                    let trendAndCycle = breakdowns |> TrendsCalculator.calculateForIndustry
                    let trend = trendAndCycle.trend
                    let lastBreakdown = breakdowns |> List.last
                    logger.LogInformation($"Saving industry {industry} trend: {trend.direction} {trend.streak} days with change of {trend.change}")
                    Storage.updateIndustryTrend lastBreakdown trend |> ignore
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
                            logger.LogInformation("Finished running")
                    with
                    | ex -> logger.LogError(ex, "Error running background service")

                    let! _ = doSleep()

                    logger.LogInformation("Finished sleeping")
            }