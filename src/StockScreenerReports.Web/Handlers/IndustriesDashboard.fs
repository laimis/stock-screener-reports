namespace StockScreenerReports.Web.Handlers

module IndustriesDashboard =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views

    type SortOrder =
        | PercentAbove200
        | PercentAbove20
        | CycleScore
        | TrendScore
        
    let sortOrderToString sortOrder =
        match sortOrder with
        | PercentAbove200 -> "percentAbove200"
        | PercentAbove20 -> "percentAbove20"
        | CycleScore -> "cycleScore"
        | TrendScore -> "trendScore"
        
    let stringToSortOrder sortOrderString =
        match sortOrderString with
        | "percentAbove200" -> PercentAbove200
        | "percentAbove20" -> PercentAbove20
        | "cycleScore" -> CycleScore
        | "trendScore" -> TrendScore
        | _ -> PercentAbove200
    
    let toBreakdownMap breakdowns =
        (breakdowns:IndustrySMABreakdown list)
        |> List.map (fun x -> (x.industry, x))
        |> Map.ofList
    
    let toTrendsMap trends =
        trends
        |> List.map (fun x -> (x.industry, x))
        |> Map.ofList

    let private generateDataRow index industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 dailyBreakdowns =
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

        let scoreCells =
            match dailyBreakdowns with
            | None -> [StringColumn(""); StringColumn(""); StringColumn("")]
            | Some dailyBreakdowns ->
            
                let cycleScore = dailyBreakdowns |> MarketCycleScoring.cycleScore
                let trendScore = dailyBreakdowns |> MarketCycleScoring.trendScore
                
                [
                    StringColumn(trendScore.ToString())
                    StringColumn(cycleScore.ToString())
                ]

        commonCells @ sma20Cells @ sma200Cells @ scoreCells

    let chartRow length dailyBreakdowns =
        
        let contents =
            match dailyBreakdowns with
            | None -> div [] [str "No data available to chart"]
            | Some dailyBreakdowns ->
                
                let smaInterval = 20
                
                let dataset : Charts.DataSet<decimal> =
                    let series = 
                        dailyBreakdowns
                        |> List.map (fun (u:IndustrySMABreakdown) -> System.Math.Round(u.breakdown.percentAbove, 0))
                        
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
        
    let private generateIndustriesView
        industrySMABreakdowns20Map
        (industrySMABreakdowns200Map:Map<string,IndustrySMABreakdown>)
        industryTrends20Map
        industryTrends200Map
        dailySMABreakdownMap
        sortFunc =
        
        let industry20And200Rows =
            industrySMABreakdowns200Map.Keys
            |> Seq.sortByDescending sortFunc
            |> Seq.indexed
            |> Seq.map (fun (index,industry) ->
                
                let industrySMABreakdown20 = industrySMABreakdowns20Map |> Map.tryFind industry
                let industrySMABreakdown200 = industrySMABreakdowns200Map |> Map.find industry
                let trend20 = industryTrends20Map |> Map.tryFind industry
                let trend200 = industryTrends200Map |> Map.tryFind industry
                let dailyBreakdowns = dailySMABreakdownMap |> Map.tryFind industry
                
                let dataCells = generateDataRow index industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 dailyBreakdowns
                let chartRow = chartRow dataCells.Length dailyBreakdowns
                
                [dataCells |> toTr; chartRow]
            )
            |> Seq.concat
            |> Seq.toList // TODO: do we truly need to convert this to list?

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
            trendScoreTm
            marketCycleScoreTm
        ]

        let table = industry20And200Rows |> fullWidthTableWithSortableHeaderCells industry20And200Header
        table
        
    let private generateSortSection algo =
        // it's a div with button links that will point to industries link with query param attached
        let sortLink selectedAlgo sortParamAsSortOrder =
            let sortParam = sortOrderToString sortParamAsSortOrder
            let href = $"?sortParam={sortParam}"
            let classes =
                match selectedAlgo = sortParamAsSortOrder with
                | true -> "button is-primary"
                | false -> "button is-info is-light"
                
            let title =
                match sortParamAsSortOrder with
                | PercentAbove200 -> "Sort by 200 SMA %"
                | PercentAbove20 -> "Sort by 20 SMA %"
                | CycleScore -> "Sort by Cycle Score"
                | TrendScore -> "Sort by Trend Score"
                
            let link = a [ _class classes; _href href ] [ str title ]
            link
            
        let sortLinks =
            [
                sortLink algo PercentAbove200
                sortLink algo PercentAbove20
                sortLink algo CycleScore
                sortLink algo TrendScore
            ]
            
        let sortLinks = sortLinks |> List.map (fun x -> div [_class "level-item"] [x])
        
        let sortSection = div [_class "level"] sortLinks
            
        sortSection

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let sortParam = ctx.GetQueryStringValue("sortParam")
            let sortAlgo =
                match sortParam with
                | Ok value -> value |> stringToSortOrder
                | Error _ -> PercentAbove200
                    
            let latestDate = Reports.getIndustrySMABreakdownLatestDate()
            let formattedDate = latestDate |> Utils.convertToDateString
            let industrySMABreakdowns20Map = Reports.getIndustrySMABreakdowns Constants.SMA20 formattedDate |> toBreakdownMap
            let industrySMABreakdowns200Map = Reports.getIndustrySMABreakdowns Constants.SMA200 formattedDate |> toBreakdownMap
            
            let industryTrends20Map = Reports.getIndustryTrends formattedDate Constants.SMA20 |> toTrendsMap 
            let industryTrends200Map = Reports.getIndustryTrends formattedDate Constants.SMA200 |> toTrendsMap
            
            let dateRange = ReportsConfig.dateRangeAsStrings()
            
            let dailySMABreakdownMap =
                industrySMABreakdowns20Map 
                |> Map.map (fun industry _ ->
                    let dailyBreakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry Constants.SMA20 dateRange
                    dailyBreakdowns
                )
                
            let sortFunc =
                match sortAlgo with
                | PercentAbove200 ->
                    fun industry ->
                        let update20 = industrySMABreakdowns20Map |> Map.tryFind industry
                        match update20 with
                        | Some update20 ->
                            let update200 = industrySMABreakdowns200Map |> Map.find industry
                            (update200.breakdown.percentAbove, update20.breakdown.percentAbove)
                        | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + industry))
                | PercentAbove20 ->
                    fun industry ->
                        let update20 = industrySMABreakdowns20Map |> Map.tryFind industry
                        match update20 with
                        | Some update20 ->
                            let update200 = industrySMABreakdowns200Map |> Map.find industry
                            (update20.breakdown.percentAbove, update200.breakdown.percentAbove)
                        | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + industry))
                | CycleScore ->
                    fun industry ->
                        let breakdowns = dailySMABreakdownMap |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let cycleScore = breakdowns |> MarketCycleScoring.cycleScore
                            (cycleScore, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                | TrendScore ->
                    fun industry ->
                        let breakdowns = dailySMABreakdownMap |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let trendScore = breakdowns |> MarketCycleScoring.trendScore
                            (trendScore, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
            
            let title = $"Industry SMA Breakdowns ({industrySMABreakdowns20Map.Count} industries) - {formattedDate}"
            
            let sortSection = generateSortSection sortAlgo
            
            let table =
                generateIndustriesView
                    industrySMABreakdowns20Map
                    industrySMABreakdowns200Map
                    industryTrends20Map
                    industryTrends200Map
                    dailySMABreakdownMap
                    sortFunc
                    
            let view = toSection title (div [] [sortSection; table])
            
            ([view] |> mainLayout $"Industries") next ctx