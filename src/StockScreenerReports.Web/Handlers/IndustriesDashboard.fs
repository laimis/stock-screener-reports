namespace StockScreenerReports.Web.Handlers

open Giraffe.ViewEngine.HtmlElements

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
        | AverageAboveRank
        | GeometricMeanRank
        | WeightedRank
        | SMACrossOverStrengthRank
        | EMACrossOverStrengthRank
        | ADXRank
        
        with static member All = [PercentAbove200; PercentAbove20; CycleScore; TrendScore; AverageAboveRank; GeometricMeanRank; WeightedRank; SMACrossOverStrengthRank; EMACrossOverStrengthRank; ADXRank]
        
    let sortAlgoToString sortAlgo =
        match sortAlgo with
        | PercentAbove200 -> nameof PercentAbove200
        | PercentAbove20 -> nameof PercentAbove20
        | CycleScore -> nameof CycleScore
        | TrendScore -> nameof TrendScore
        | AverageAboveRank -> nameof AverageAboveRank
        | GeometricMeanRank -> nameof GeometricMeanRank
        | WeightedRank -> nameof WeightedRank
        | SMACrossOverStrengthRank -> nameof SMACrossOverStrengthRank
        | EMACrossOverStrengthRank -> nameof EMACrossOverStrengthRank
        | ADXRank -> nameof ADXRank
        
    let stringToSortAlgo sortAlgoString =
        match sortAlgoString with
        | nameof PercentAbove200 -> PercentAbove200
        | nameof PercentAbove20 -> PercentAbove20
        | nameof CycleScore -> CycleScore
        | nameof TrendScore -> TrendScore
        | nameof AverageAboveRank -> AverageAboveRank
        | nameof GeometricMeanRank -> GeometricMeanRank
        | nameof WeightedRank -> WeightedRank
        | nameof SMACrossOverStrengthRank -> SMACrossOverStrengthRank
        | nameof EMACrossOverStrengthRank -> EMACrossOverStrengthRank
        | nameof ADXRank -> ADXRank
        | _ -> CycleScore
        
    let sortAlgoToLabel sortAlgo =
        match sortAlgo with
        | PercentAbove200 -> "200 SMA %"
        | PercentAbove20 -> "20 SMA %"
        | TrendScore -> "Trend Score"
        | CycleScore -> "Cycle Score"
        | AverageAboveRank -> "Average SMA %"
        | GeometricMeanRank -> "Geometric Mean Rank"
        | WeightedRank -> "Weighted Rank"
        | SMACrossOverStrengthRank -> "SMA Cross Over Strength Rank"
        | EMACrossOverStrengthRank -> "EMA Cross Over Strength Rank"
        | ADXRank -> "ADX Rank"
    
    let toBreakdownMap breakdowns =
        (breakdowns:IndustrySMABreakdown list)
        |> List.map (fun x -> (x.industry, x))
        |> Map.ofList
    
    let toTrendsMap trends =
        trends
        |> List.map (fun (x:IndustryTrend) -> (x.industry, x))
        |> Map.ofList
        
    let private generateDataDiv industrySMABreakdown20 industrySMABreakdown200 trend20 trend200 =
        let toSMACells (sma:SMA) (smaBreakdown:IndustrySMABreakdown) (trendOption:Option<IndustryTrend>) =
            
            let trend =
                match trendOption with
                | Some trend -> trend.trend
                | None -> Trend.blank()

            [
                $"{sma.Interval} sma", $"{smaBreakdown.breakdown.above} / {smaBreakdown.breakdown.total}"
                $"{sma.Interval} sma %%", System.String.Format("{0:N2}%", smaBreakdown.breakdown.percentAbove)
                "Trend Change", trend.changeFormatted
                "Trend Streak", trend.streakFormatted
                "Rate", trend.streakRateFormatted
            ]
        
        let toLevelItem title value =
            div [_class "level-item has-text-centered"] [
                div [] [
                    div [_class "heading"] [str title]
                    div [_class "title is-size-5"] [str value]
                ]
            ]
                
        [
            toSMACells SMA20 industrySMABreakdown20 trend20
            toSMACells SMA200 industrySMABreakdown200 trend200
        ]
        |>
        List.map (fun pairs ->
            pairs |> List.map (fun (title, value) -> toLevelItem title value) |> div [_class "level"]
        )
        |> div [_class "box"] 
        

    let chartSection dailyBreakdowns20 dailyBreakdowns200 =
        
        match dailyBreakdowns20, dailyBreakdowns200 with
        | Some dailyBreakdowns20, Some dailyBreakdowns200 ->
            
            let generateSeries (sma:SMA) breakdowns : Charts.DataSet<decimal> =
                let series = 
                    breakdowns
                    |> List.map (fun (u:IndustrySMABreakdown) -> System.Math.Round(u.breakdown.percentAbove, 0))
                    
                {
                    data = series
                    title = $"{sma.Interval} SMA Trend"
                    color = sma.Color
                }
                
            let dataSets =
                [
                    generateSeries SMA20 dailyBreakdowns20
                    generateSeries SMA200 dailyBreakdowns200
                ]
                
            let labels = dailyBreakdowns20 |> List.map (fun u -> u.breakdown.date |> Utils.formatDateForChart)
            
            let chartElements =
                dataSets |> Charts.generateChartElements "sma breakdown chart" Charts.ChartType.Line (Some 100) Charts.smallChart labels
            
            div [] chartElements
        | None, _ -> div [] [str "Missing sma 20 data "]
        | _, None -> div [] [str "Missing sma 200 data "]
        
    let private generateIndustriesView
        industrySMABreakdowns20Map
        (industrySMABreakdowns200Map:Map<string,IndustrySMABreakdown>)
        industryTrends20Map
        industryTrends200Map
        dailySMABreakdown20Map
        dailySMABreakdown200Map
        sortAlgo
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
                let dailyBreakdowns20 = dailySMABreakdown20Map |> Map.tryFind industry
                let dailyBreakdowns200 = dailySMABreakdown200Map |> Map.tryFind industry
                
                let dataDiv = generateDataDiv industrySMABreakdown20 industrySMABreakdown200 trend20 trend200
                let chartDiv = chartSection dailyBreakdowns20 dailyBreakdowns200
                
                let trendScore, cycleScore, sortScore =
                    match dailyBreakdowns20 with
                    | None -> 0m, 0m, 0m
                    | Some dailyBreakdowns ->
                    
                        let trendScore = dailyBreakdowns |> MarketCycleScoring.trendScore
                        let cycleScore = dailyBreakdowns |> MarketCycleScoring.cycleScore
                        let sortScore = sortFunc industry |> fst
                        
                        trendScore, cycleScore, sortScore
                        
                let selectedScoreLabel = sortAlgoToLabel sortAlgo
                
                div [_class "box"] [
                    header [_class "title is-4"] [
                        div [_class "level"] [
                            
                            div [_class "level-item"] [
                                $"{index + 1}." |> str
                            ]
                            div [_class "level-item"] [
                                industry |> Links.industryLink |> generateHref industry
                            ]
                            div [_class "level-item has-text-centered"] [
                                $"{trendScoreTm} {trendScore}" |> str
                            ]
                            div [_class "level-item has-text-centered"] [
                                $"{marketCycleScoreTm} {cycleScore}" |> str
                            ]
                            div [_class "level-item"] [
                                $"{selectedScoreLabel} {sortScore}" |> str
                            ]
                            div [ _class "level-right is-size-6" ] [
                                div [_class "level-item"] [
                                    industry |> Links.industryFinvizLink |> generateHrefNewTab "finviz"
                                ]
                            ]
                        ]
                    ]
                    div [_class "card-content"] [
                        dataDiv
                        chartDiv
                    ]
                ]
            )
            |> Seq.toList
            
        div [] industry20And200Rows
        
    
    let private generateHref sortAlgo minimumStocks =
        let sortAlgoAsString = sortAlgoToString sortAlgo
        let href = $"?sortParam={sortAlgoAsString}&minimumStocks={minimumStocks}"
        href
        
    let private generateSortAndFilterSection selectedAlgo selectedMinimumNumberOfStocks =
        let sortOptions = SortAlgo.All |> List.map (fun x -> x, x |> sortAlgoToLabel) 

        let filterOptions =
            [
                0, "Any number of stocks"
                5, "With at least 5 stocks"
                10, "With at least 10 stocks"
                20, "With at least 20 stocks"
            ]
            
        let onChangeAttribute = XmlAttribute.KeyValue("onchange", "location = this.value;")

        let sortDropdown =
            div [ _class "select" ] [
                select [ onChangeAttribute ] [
                    for sortAlgo, title in sortOptions do
                        let href = generateHref sortAlgo selectedMinimumNumberOfStocks
                        let isSelected = selectedAlgo = sortAlgo
                        option [
                            _value href
                            if isSelected then _selected
                        ] [ str title ]
                ]
            ]

        let filterDropdown =
            div [ _class "select" ] [
                select [ onChangeAttribute ] [
                    for selection, title in filterOptions do
                        let href = generateHref selectedAlgo selection
                        let isSelected = selectedMinimumNumberOfStocks = selection
                        option [
                            _value href
                            if isSelected then _selected
                        ] [ str title ]
                ]
            ]

        let sortAndFilterSection =
            div [ _class "level" ] [
                div [ _class "level-left" ] [
                    div [ _class "level-item" ] [
                        div [ _class "field" ] [
                            label [ _class "label" ] [ str "Sort" ]
                            div [ _class "control" ] [
                                sortDropdown
                            ]
                        ]
                    ]
                ]
                div [ _class "level-right" ] [
                    div [ _class "level-item" ] [
                        div [_class "field"] [
                            label [ _class "label" ] [ str "Filter" ]
                            div [ _class "control" ] [
                                filterDropdown
                            ]
                        ]
                    ]
                ]
            ]

        sortAndFilterSection

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
                Reports.getIndustrySMABreakdownsForDate SMA20 formattedDate
                |> toBreakdownMap
                |> Map.filter (fun _ breakdown -> breakdown.breakdown.total >= minimumStocks)
                
            let industrySMABreakdowns200Map =
                Reports.getIndustrySMABreakdownsForDate SMA200 formattedDate
                |> toBreakdownMap
                |> Map.filter (fun _ breakdown -> breakdown.breakdown.total >= minimumStocks)
            
            let industryTrends20Map = Reports.getIndustryTrends formattedDate SMA20 |> toTrendsMap 
            let industryTrends200Map = Reports.getIndustryTrends formattedDate SMA200 |> toTrendsMap
            
            let dateRange = ReportsConfig.dateRangeAsStrings()
            
            let dailySMABreakdown20Map =
                industrySMABreakdowns20Map 
                |> Map.map (fun industry _ ->
                    let dailyBreakdowns = industry |> Reports.getIndustrySMABreakdownsForDateRange SMA20 dateRange
                    dailyBreakdowns
                )
                
            let dailySMABreakdown200Map =
                industrySMABreakdowns20Map 
                |> Map.map (fun industry _ ->
                    let dailyBreakdowns = industry |> Reports.getIndustrySMABreakdownsForDateRange SMA200 dateRange
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
                        let breakdowns = dailySMABreakdown20Map |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let trendScore = breakdowns |> MarketCycleScoring.trendScore
                            (trendScore, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                | CycleScore ->
                    fun industry ->
                        let breakdowns = dailySMABreakdown20Map |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let cycleScore = breakdowns |> MarketCycleScoring.cycleScore
                            (cycleScore, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                | SMACrossOverStrengthRank ->
                    fun industry ->
                        let breakdowns = dailySMABreakdown20Map |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns -> (breakdowns |> TrendsCalculator.calculateSMACrossOverStrength, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                | EMACrossOverStrengthRank ->
                    fun industry ->
                        let breakdowns = dailySMABreakdown20Map |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns -> (breakdowns |> TrendsCalculator.calculateEMACrossOverStrength, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                | ADXRank ->
                    fun industry ->
                        let breakdowns = dailySMABreakdown20Map |> Map.tryFind industry
                        match breakdowns with
                        | Some breakdowns ->
                            let adxRank = breakdowns |> TrendsCalculator.calculateADXTrend
                            (adxRank, 0m)
                        | None -> raise (System.Exception("Could not find daily breakdowns for " + industry))
                        
                | AverageAboveRank ->
                    fun industry ->
                        let update20 = industrySMABreakdowns20Map |> Map.tryFind industry
                        match update20 with
                        | Some update20 ->
                            let update200 = industrySMABreakdowns200Map |> Map.find industry
                            let average = TrendsCalculator.calculateAverageOfSMA20AndSMA200 update20 update200
                            (average, 0m)
                        | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + industry))
                | GeometricMeanRank ->
                    fun industry ->
                        let update20 = industrySMABreakdowns20Map |> Map.tryFind industry
                        match update20 with
                        | Some update20 ->
                            let update200 = industrySMABreakdowns200Map |> Map.find industry
                            let geometricMean = TrendsCalculator.calculateGeometricMeanOfSMA20AndSMA200 update20 update200
                            (geometricMean, 0m)
                        | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + industry))
                | WeightedRank ->
                    fun industry ->
                        let update20 = industrySMABreakdowns20Map |> Map.tryFind industry
                        match update20 with
                        | Some update20 ->
                            let update200 = industrySMABreakdowns200Map |> Map.find industry
                            let weighted = TrendsCalculator.calculateWeightedRankOfSMA20AndSMA200 update20 update200
                            (weighted, 0m)
                        | None -> raise (System.Exception("Could not find 20 day SMA breakdown for " + industry))
            
            let title = $"Industry SMA Breakdowns ({industrySMABreakdowns20Map.Count} industries) - {formattedDate}"
            
            let sortAndFilterSection = generateSortAndFilterSection sortAlgo minimumStocks
            
            let view =
                generateIndustriesView
                    industrySMABreakdowns20Map
                    industrySMABreakdowns200Map
                    industryTrends20Map
                    industryTrends200Map
                    dailySMABreakdown20Map
                    dailySMABreakdown200Map
                    sortAlgo
                    sortFunc
                    
            let view = toSection title (div [_class "content"] [sortAndFilterSection; view])
            
            ([view] |> mainLayout $"Industries") next ctx