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
        let fetchScreenerResults (input:StockScreenerReports.Core.Screener) =
            logger.LogInformation(sprintf "Running screener %s" input.name)
            let results = FinvizClient.getResults input.url
            (input,results)

        let saveToDb (screenerResults:list<StockScreenerReports.Core.Screener * 'a>) =
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

            let message = $"Ran {results.Length} screeners successfully"

            Storage.saveJobStatus ScreenerJob (ReportsConfig.nowUtcNow()) Success message |> ignore

        runIfTradingDay funcToRun

    let earningsRun (logger:ILogger) =
        
        let funcToRun() =
            let earnings = FinvizClient.getEarnings()

            let message = $"Ran {earnings.Length} earnings successfully"

            logger.LogInformation(message)

            earnings
                |> List.iter (fun x ->
                    let (ticker,earningsTime) = x
                    Storage.saveEarningsDate ticker (Utils.getRunDate()) earningsTime |> ignore
                )

            Storage.saveJobStatus EarningsJob (ReportsConfig.nowUtcNow()) Success message |> ignore

        runIfTradingDay funcToRun

    let trendsRun (logger:ILogger) =
        let funcToRun() =
            let date = Utils.getRunDate()

            logger.LogInformation($"Running trends for {date}")
        
            // pull above and below 20 and 200 for each industry, and store the results
            let knownIndustries = Storage.getIndustries()
            let smas = Constants.SMAS

            let industrySmaPairs = knownIndustries |> Seq.map (fun industry -> smas |> List.map (fun sma -> (industry, sma))) |> Seq.concat

            let industriesUpdated =
                industrySmaPairs
                |> Seq.map (fun (industry,sma) ->
                    logger.LogInformation($"Processing industry {industry} {sma} day sma breakdown")
                    (industry,sma))
                |> Seq.map (fun (industry, sma) ->   
                    let (above,below) = industry |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA sma
                    (industry, sma, above, below)
                )
                |> Seq.map (fun r ->
                    Storage.saveIndustrySMABreakdowns date r |> ignore
                )
                |> Seq.length

            // updating breakdowns
            smas |> List.iter (fun days -> Storage.updateSMABreakdowns date days |> ignore)

            logger.LogInformation($"Calculating trends")

            let trendsUpdated =
                industrySmaPairs
                |> Seq.map (fun (industry, days) -> 
                    
                    let breakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry days (ReportsConfig.dateRangeAsStrings())
                    let trendAndCycle = breakdowns |> TrendsCalculator.calculateTrendAndCycleForIndustry
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

        // f# try/catch

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

        let runTracker = new HashSet<string>()

        let doSleepInternal (sleepTime:int) =
            logger.LogInformation($"Sleeping for {sleepTime} milliseconds")
            System.Threading.Tasks.Task.Delay(sleepTime)
            
        let doSleep() =
            60 * 60 * 1000 |> doSleepInternal

        let shortSleep() =
            10 * 1000 |> doSleepInternal

        override __.ExecuteAsync(cancellationToken) =
            task {
                while cancellationToken.IsCancellationRequested |> not do
                    
                        
                logger.LogInformation("---- background service")
                let now = ReportsConfig.now()
                let time = now.TimeOfDay
                let marketClose = new TimeSpan(16,0,0)
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
                            logger.LogInformation("Finished running")
                    with
                    | ex -> logger.LogError(ex, "Error running background service")

                    let! _ = doSleep()

                    logger.LogInformation("Finished sleeping")
            }