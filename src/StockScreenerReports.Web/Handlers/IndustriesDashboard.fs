namespace StockScreenerReports.Web.Handlers

module IndustriesDashboard =
    open Giraffe
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
            
    let private generateIndustrySMATable() =
        let latestDate = Reports.getIndustrySMABreakdownLatestDate()
        let formattedDate = latestDate |> Utils.convertToDateString

        let thirtyDaysAgo = Utils.addDaysToClosestBusinessDay latestDate -30 |> Utils.convertToDateString
        let sixtyDaysAgo = Utils.addDaysToClosestBusinessDay latestDate -60 |> Utils.convertToDateString

        let getIndustrySMABreakdownsAndTurnToMap date (days:int) =
            date 
            |> Reports.getIndustrySMABreakdowns days
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList
        
        let industrySMABreakdowns20 = getIndustrySMABreakdownsAndTurnToMap formattedDate 20
        let industrySMABreakdown200 = getIndustrySMABreakdownsAndTurnToMap formattedDate 200
        let smaBreakdown200_30days = getIndustrySMABreakdownsAndTurnToMap thirtyDaysAgo 200
        let smaBreakdown200_60days = getIndustrySMABreakdownsAndTurnToMap sixtyDaysAgo 200

        let getIndustryTrendsAndTurnToMap date (days:int) =
            days
            |> Reports.getIndustryTrends date
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList

        let industryTrend20 = getIndustryTrendsAndTurnToMap formattedDate 20
        let industryTrend200 = getIndustryTrendsAndTurnToMap formattedDate 200

        let industry20And200Rows =
            industrySMABreakdown200
            |> Map.toList
            |> List.sortByDescending (fun (key, update200) -> 
                let update20Option = industrySMABreakdowns20 |> Map.tryFind key
                match update20Option with
                | Some update20 -> (update200.breakdown.percentAbove, update20.breakdown.percentAbove)
                | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + key))
            )
            |> List.indexed
            |> List.map (fun (index,(industryName, iu)) ->

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
                        $"{smaBreakdown.breakdown.above} / {smaBreakdown.breakdown.total}" |> toTd
                        System.String.Format("{0:N2}%", smaBreakdown.breakdown.percentAbove) |> toTd
                        trend.changeFormatted |> toTd
                        trend.streakFormatted |> toTd
                        trend.streakRateFormatted |> toTd
                    ]

                let smaBreakdown = industrySMABreakdowns20 |> Map.tryFind industryName
                let trend20 = industryTrend20 |> Map.tryFind industryName
                let trend200 = industryTrend200 |> Map.tryFind industryName
                
                let sma20Cells = toSMACells smaBreakdown trend20
                let sma200Cells = toSMACells (Some iu) trend200

                let breakdownDiff breakdownMap =
                    let toCompare =
                        match (breakdownMap |> Map.tryFind industryName) with
                        | Some update -> update
                        | None -> iu

                    let diff = iu.breakdown.percentAbove - toCompare.breakdown.percentAbove
                    
                    System.Math.Round(diff, 0).ToString() |> toTd

                let industryLinks = [
                    span [ _class "mr-2"] [
                            iu.industry |> Links.industryLink |> generateHref iu.industry
                        ]
                        
                    span [ _class "is-pulled-right"] [
                        iu.industry |> Links.industryFinvizLink |> generateHrefNewTab "finviz" 
                    ]
                ]

                let commonCells = [
                    (index+1).ToString() |> toTd
                    industryLinks |> toTdWithNodes
                ]

                // TODO: precalculate these, it can get expensive
                let smaBreakdowns = 
                    industryName
                    |> Reports.getIndustrySMABreakdownsForIndustry Constants.SMA20 ReportsConfig.dateRangeAsStrings

                let cycleScoreComponents = 
                    smaBreakdowns |> MarketCycleScoring.cycleScoreComponents
                    
                let interestScore1 =
                    cycleScoreComponents |> MarketCycleScoring.componentScoreAdding 

                let interestScore2 = 
                    cycleScoreComponents |> MarketCycleScoring.componentScoreMultiplying

                let diffCells = [
                    interestScore1.ToString() |> toTd
                    interestScore2.ToString() |> toTd
                    breakdownDiff smaBreakdown200_30days
                    breakdownDiff smaBreakdown200_60days
                ]

                let cells = commonCells @ sma20Cells @ sma200Cells @ diffCells

                tr [] cells
            )

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
            (marketCycleScoreTm + "+")
            (marketCycleScoreTm + "*") 
            "30 diff"
            "60 diff"
        ]

        industry20And200Rows |> fullWidthTableWithSortableHeaderCells industry20And200Header

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let industriesTable = generateIndustrySMATable()

            let jobStatusRow = IndustryTrendsJob |> Utils.genericJobStatusGet |> generateJobStatusDiv

            let view = [
                section [_class "content"] [
                    h1 [] [str "Industry SMA Breakdowns"]
                    industriesTable
                ]
                jobStatusRow
            ]
            
            (view |> mainLayout $"Industries") next ctx