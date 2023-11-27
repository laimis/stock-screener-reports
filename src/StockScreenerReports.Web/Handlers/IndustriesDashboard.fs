namespace StockScreenerReports.Web.Handlers

module IndustriesDashboard =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views

    type SortAlgo =
        | PercentAbove200
        | PercentAbove20
        | CycleScore
        | TrendScore
        
    let sortAlgoToString sortAlgo =
        match sortAlgo with
        | PercentAbove200 -> "percentAbove200"
        | PercentAbove20 -> "percentAbove20"
        | CycleScore -> "cycleScore"
        | TrendScore -> "trendScore"
        
    let stringToSortAlgo sortAlgoString =
        match sortAlgoString with
        | "percentAbove200" -> PercentAbove200
        | "percentAbove20" -> PercentAbove20
        | "cycleScore" -> CycleScore
        | "trendScore" -> TrendScore
        | _ -> CycleScore
    
    let toBreakdownMap breakdowns =
        (breakdowns:IndustrySMABreakdown list)
        |> List.map (fun x -> (x.industry, x))
        |> Map.ofList
    
    let toTrendsMap trends =
        trends
        |> List.map (fun x -> (x.industry, x))
        |> Map.ofList

    let private generateDataRow index industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 dailyBreakdowns =
        let toSMACells (smaBreakdown:IndustrySMABreakdown) (trendOption:Option<IndustryTrend>) =
            
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
        let sma200Cells = toSMACells industrySMABreakdown200 trend200

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
            
                let trendScore = dailyBreakdowns |> MarketCycleScoring.trendScore
                let cycleScore = dailyBreakdowns |> MarketCycleScoring.cycleScore
                
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
                
                let sma = SMA20
                
                let dataset : Charts.DataSet<decimal> =
                    let series = 
                        dailyBreakdowns
                        |> List.map (fun (u:IndustrySMABreakdown) -> System.Math.Round(u.breakdown.percentAbove, 0))
                        
                    {
                        data = series
                        title = $"{sma |> SMA.toInterval} SMA Trend"
                        color = sma |> SMA.toColor
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
                
                let industrySMABreakdown20 = industrySMABreakdowns20Map |> Map.find industry
                let industrySMABreakdown200 = industrySMABreakdowns200Map |> Map.find industry
                let trend20 = industryTrends20Map |> Map.tryFind industry
                let trend200 = industryTrends200Map |> Map.tryFind industry
                let dailyBreakdowns = dailySMABreakdownMap |> Map.tryFind industry
                
                let dataCells = generateDataRow index industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 dailyBreakdowns
                let chartRow = chartRow dataCells.Length dailyBreakdowns
                
                [dataCells |> toTr; chartRow]
            )
            |> Seq.concat
            |> Seq.toList

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
    
    let private generateHref sortAlgo minimumStocks =
        let sortAlgoAsString = sortAlgoToString sortAlgo
        let href = $"?sortParam={sortAlgoAsString}&minimumStocks={minimumStocks}"
        href
        
    let private generateSortSection selectedAlgo selectedMinimumNumberOfStocks =
        
        let sortLink sortAlgo =
            let href = generateHref sortAlgo selectedMinimumNumberOfStocks
            let classes =
                match selectedAlgo = sortAlgo with
                | true -> "button is-primary"
                | false -> "button is-info is-light"
                
            let title =
                match sortAlgo with
                | PercentAbove200 -> "Sort by 200 SMA %"
                | PercentAbove20 -> "Sort by 20 SMA %"
                | TrendScore -> "Sort by Trend Score"
                | CycleScore -> "Sort by Cycle Score"
                
            a [ _class classes; _href href ] [ str title ]
            
        let sortLinks =
            [
                PercentAbove200
                PercentAbove20
                TrendScore
                CycleScore
            ] |> List.map sortLink
            
        let sortLinks = sortLinks |> List.map (fun sortLink -> div [_class "level-item"] [sortLink])
        
        let sortSection = div [_class "level"] sortLinks
            
        sortSection
        
    let private filterSection selectedAlgo minimumSelected =
        
        let filterLink selection =
            let href = generateHref selectedAlgo selection
            let classes =
                match minimumSelected = selection with
                | true -> "button is-primary"
                | false -> "button is-info is-light"
                
            let title =
                match selection with
                | x when x > 0 -> $"With at least {selection} stocks"
                | 0 -> "Any number of stocks"
                | _ -> raise (System.Exception("Invalid selection"))
                
            a [ _class classes; _href href ] [ str title ]
            
        let filterLinks = [0; 5; 10; 20] |> List.map filterLink
            
        let filterLinks = filterLinks |> List.map (fun sortLink -> div [_class "level-item"] [sortLink])
        
        div [_class "level"] filterLinks

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let sortParam = ctx.GetQueryStringValue("sortParam")
            let sortAlgo =
                match sortParam with
                | Ok value -> value |> stringToSortAlgo
                | Error _ -> CycleScore
                
            let filterParam = ctx.GetQueryStringValue("minimumStocks")
            let minimumStocks =
                match filterParam with
                | Ok value -> value |> int
                | Error _ -> 0
                    
            let latestDate = Reports.getIndustrySMABreakdownLatestDate()
            let formattedDate = latestDate |> Utils.convertToDateString
            let industrySMABreakdowns20Map =
                Reports.getIndustrySMABreakdowns SMA20 formattedDate
                |> toBreakdownMap
                |> Map.filter (fun _ breakdown -> breakdown.breakdown.total >= minimumStocks)
                
            let industrySMABreakdowns200Map =
                Reports.getIndustrySMABreakdowns SMA200 formattedDate
                |> toBreakdownMap
                |> Map.filter (fun _ breakdown -> breakdown.breakdown.total >= minimumStocks)
            
            let industryTrends20Map = Reports.getIndustryTrends formattedDate SMA20 |> toTrendsMap 
            let industryTrends200Map = Reports.getIndustryTrends formattedDate SMA200 |> toTrendsMap
            
            let dateRange = ReportsConfig.dateRangeAsStrings()
            
            let dailySMABreakdownMap =
                industrySMABreakdowns20Map 
                |> Map.map (fun industry _ ->
                    let dailyBreakdowns = industry |> Reports.getIndustrySMABreakdownsForIndustry SMA20 dateRange
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
                | TrendScore ->
                    fun industry ->
                        let breakdowns = dailySMABreakdownMap |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let trendScore = breakdowns |> MarketCycleScoring.trendScore
                            (trendScore, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                | CycleScore ->
                    fun industry ->
                        let breakdowns = dailySMABreakdownMap |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let cycleScore = breakdowns |> MarketCycleScoring.cycleScore
                            (cycleScore, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
            
            let title = $"Industry SMA Breakdowns ({industrySMABreakdowns20Map.Count} industries) - {formattedDate}"
            
            let sortSection = generateSortSection sortAlgo minimumStocks
            let filterSection = filterSection sortAlgo minimumStocks
            
            let table =
                generateIndustriesView
                    industrySMABreakdowns20Map
                    industrySMABreakdowns200Map
                    industryTrends20Map
                    industryTrends200Map
                    dailySMABreakdownMap
                    sortFunc
                    
            let view = toSection title (div [] [sortSection; filterSection; table])
            
            ([view] |> mainLayout $"Industries") next ctx