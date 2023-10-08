namespace StockScreenerReports.Web.Handlers

module IndustriesDashboard =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views

    
    let private generateDataRow index industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 industrySMABreakdown200_30days industrySMABreakdown200_60days dailyBreakdowns =
        let toSMACells (smaOption:Option<IndustrySMABreakdown>) (trendOption:Option<IndustryTrend>) =
            let smaBreakdown =
                match smaOption with
                | Some sma -> sma
                | None -> (IndustrySMABreakdown.blank "NA")

            let trend =
                match trendOption with
                | Some trend -> trend.trend
                | None -> Trend.blank()

            [
                StringColumn($"{smaBreakdown.breakdown.above} / {smaBreakdown.breakdown.total}")
                StringColumn(System.String.Format("{0:N2}%", smaBreakdown.breakdown.percentAbove))
                StringColumn(trend.changeFormatted)
                StringColumn(trend.streakFormatted)
                StringColumn(trend.streakRateFormatted)
            ]

        let sma20Cells = toSMACells industrySMABreakdown20 trend20
        let sma200Cells = toSMACells (Some industrySMABreakdown200) trend200

        let breakdownDiff toCompare =
            let toCompare =
                match toCompare with
                | Some update -> update
                | None -> industrySMABreakdown200

            let diff = industrySMABreakdown200.breakdown.percentAbove - toCompare.breakdown.percentAbove
            
            StringColumn(System.Math.Round(diff, 0).ToString())

        let industryLinks = div [] [
            span [ _class "mr-2"] [
                    industrySMABreakdown200.industry |> Links.industryLink |> generateHref industrySMABreakdown200.industry
                ]
                
            span [ _class "is-pulled-right"] [
                industrySMABreakdown200.industry |> Links.industryFinvizLink |> generateHrefNewTab "finviz" 
            ]
        ]

        let commonCells = [
            StringColumn((index+1).ToString())
            NodeColumn(industryLinks)
        ]

        let diffCells =
            match dailyBreakdowns with
            | None -> [StringColumn(""); StringColumn(""); StringColumn("")]
            | Some dailyBreakdowns ->
            
                let cycleScore = 
                    dailyBreakdowns
                    |> MarketCycleScoring.cycleScoreComponents
                    |> MarketCycleScoring.componentScore
                
                [
                    StringColumn(cycleScore.ToString())
                    breakdownDiff industrySMABreakdown200_30days
                    breakdownDiff industrySMABreakdown200_60days
                ]

        commonCells @ sma20Cells @ sma200Cells @ diffCells

    let chartRow length dailyBreakdowns =
        
        let contents =
            match dailyBreakdowns with
            | None -> div [] [str "No data available to chart"]
            | Some dailyBreakdowns ->
                
                let smaInterval = 20
                
                let dataset : Charts.DataSet<decimal> =
                    let series = 
                        dailyBreakdowns
                        |> List.map (fun u -> System.Math.Round(u.breakdown.percentAbove, 0))
                        
                    {
                        data = series
                        title = $"{smaInterval} SMA Trend"
                        color = smaInterval |> Constants.mapSmaToColor
                    }
                    
                    
                let labels = dailyBreakdowns |> List.map (fun u -> u.breakdown.date.ToString("MMM/dd"))
                
                let chartElements =
                    [dataset] |> Charts.generateChartElements "sma breakdown chart" Charts.ChartType.Line (Some 100) Charts.smallChart labels
                
                div [] chartElements
        
        tr [] [
            td [_colspan (length.ToString())] [
                contents
            ]
        ]
        
    let private generateIndustrySMATable
        industrySMABreakdowns20
        industrySMABreakdowns200
        industrySMABreakdowns30Days200
        industrySMABreakdowns60Days200
        industryTrends20
        industryTrends200
        dailySMABreakdownMap =
        
        let turnToMap breakdowns =
            (breakdowns:IndustrySMABreakdown list)
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList
        
        let industrySMABreakdowns20 = turnToMap industrySMABreakdowns20
        let smaBreakdown200_30days = turnToMap industrySMABreakdowns30Days200
        let smaBreakdown200_60days = turnToMap industrySMABreakdowns60Days200

        let trendsTurnToMap trends =
            trends
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList

        let industryTrend20 = trendsTurnToMap industryTrends20
        let industryTrend200 = trendsTurnToMap industryTrends200

        let industry20And200Rows =
            industrySMABreakdowns200
            |> List.sortByDescending (fun (industrySMABreakdown200:IndustrySMABreakdown) -> 
                let update20 = industrySMABreakdowns20 |> Map.tryFind industrySMABreakdown200.industry
                match update20 with
                | Some update20 -> (industrySMABreakdown200.breakdown.percentAbove, update20.breakdown.percentAbove)
                | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + industrySMABreakdown200.industry))
            )
            |> List.indexed
            |> List.map (fun (index,industrySMABreakdown200) ->
                
                let industrySMABreakdown20 = industrySMABreakdowns20 |> Map.tryFind industrySMABreakdown200.industry
                let trend20 = industryTrend20 |> Map.tryFind industrySMABreakdown200.industry
                let trend200 = industryTrend200 |> Map.tryFind industrySMABreakdown200.industry
                let dailyBreakdowns = dailySMABreakdownMap |> Map.tryFind industrySMABreakdown200.industry
                let industrySMABreakdown200_30days = smaBreakdown200_30days |> Map.tryFind industrySMABreakdown200.industry
                let industrySMABreakdown200_60days = smaBreakdown200_60days |> Map.tryFind industrySMABreakdown200.industry
                
                let dataCells = generateDataRow index industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 industrySMABreakdown200_30days industrySMABreakdown200_60days dailyBreakdowns
                let chartRow = chartRow dataCells.Length dailyBreakdowns
                
                [dataCells |> toTr; chartRow]
            )
            |> List.concat

        let industry20And200Header = [
            ""
            "Industry"
            "20 sma"
            "20 sma %"
            "Trend Change"
            "Trend Streak"
            "Rate"
            "200 sma"
            "200 sma %"
            "Trend Change"
            "Trend Streak"
            "Rate"
            marketCycleScoreTm
            "30 diff"
            "60 diff"
        ]

        industry20And200Rows |> fullWidthTableWithSortableHeaderCells industry20And200Header

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let latestDate = Reports.getIndustrySMABreakdownLatestDate()
            let formattedDate = latestDate |> Utils.convertToDateString

            let thirtyDaysAgo = Utils.addDaysToClosestBusinessDay latestDate -30 |> Utils.convertToDateString
            let sixtyDaysAgo = Utils.addDaysToClosestBusinessDay latestDate -60 |> Utils.convertToDateString
            
            let industrySMABreakdowns20 = Reports.getIndustrySMABreakdowns Constants.SMA20 formattedDate
            let industrySMABreakdowns200 = Reports.getIndustrySMABreakdowns Constants.SMA200 formattedDate
            let industrySMABreakdowns30Days200 = Reports.getIndustrySMABreakdowns Constants.SMA200 thirtyDaysAgo
            let industrySMABreakdowns60Days200 = Reports.getIndustrySMABreakdowns Constants.SMA200 sixtyDaysAgo
            
            let industryTrends20 = Reports.getIndustryTrends formattedDate Constants.SMA20
            let industryTrends200 = Reports.getIndustryTrends formattedDate Constants.SMA200

            let dailySMABreakdownMap =
                industrySMABreakdowns20
                |> List.map (fun x ->
                    let dailyBreakdowns = x.industry |> Reports.getIndustrySMABreakdownsForIndustry Constants.SMA20 (ReportsConfig.dateRangeAsStrings())
                    (x.industry, dailyBreakdowns)
                )
                |> Map.ofList 

            let title = $"Industry SMA Breakdowns ({industrySMABreakdowns20.Length} industries) - {formattedDate}"
            
            let view = generateIndustrySMATable industrySMABreakdowns20 industrySMABreakdowns200 industrySMABreakdowns30Days200 industrySMABreakdowns60Days200 industryTrends20 industryTrends200 dailySMABreakdownMap |> toSection title
            
            ([view] |> mainLayout $"Industries") next ctx