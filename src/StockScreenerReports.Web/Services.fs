namespace StockScreenerReports.Web

module Services =
    open StockScreenerReports.Storage
    open Microsoft.Extensions.Logging
    open StockScreenerReports.Core
    open StockScreenerReports.FinvizClient

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

        let results =
            Storage.getScreeners()
            |> List.map fetchScreenerResults
            |> saveToDb

        let message = $"Ran {results.Length} screeners successfully"

        Storage.saveJobStatus ScreenerJob (ReportsConfig.nowUtcNow()) Success message |> ignore

    let earningsRun (logger:ILogger) =
        let earnings = FinvizClient.getEarnings()

        let message = $"Ran {earnings.Length} earnings successfully"

        logger.LogInformation(message)

        earnings
            |> List.iter (fun x ->
                let (ticker,earningsTime) = x
                Storage.saveEarningsDate ticker (Utils.getRunDate()) earningsTime |> ignore
            )

        Storage.saveJobStatus EarningsJob (ReportsConfig.nowUtcNow()) Success "Ran earnings successfully" |> ignore

    let trendsRun (logger:ILogger) =
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
