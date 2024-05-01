namespace StockScreenerReports.Web.Handlers

module IndustriesTable =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    
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
    
    let private generateHref minimumStocks =
        let href = $"?minimumStocks={minimumStocks}"
        href
        
    let private generateInfoAndFilterSection matchCount selectedMinimumNumberOfStocks =
        
        let filterOptions =
            [
                0, "Any number of stocks"
                5, "With at least 5 stocks"
                10, "With at least 10 stocks"
                20, "With at least 20 stocks"
            ]
            
        let onChangeAttribute = XmlAttribute.KeyValue("onchange", "location = this.value;")

        let filterDropdown =
            div [ _class "select" ] [
                select [ onChangeAttribute ] [
                    for selection, title in filterOptions do
                        let href = generateHref selection
                        let isSelected = selectedMinimumNumberOfStocks = selection
                        option [
                            _value href
                            if isSelected then _selected
                        ] [ str title ]
                ]
            ]

        let infoAndFilterSection =
            div [ _class "level" ] [
                div [ _class "level-left" ] [
                    div [ _class "level-item" ] [
                        span [] [ str $"{matchCount} Industries" ]
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

        infoAndFilterSection

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            
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
            
            let dateRange = ReportsConfig.dateRangeAsStrings()
            
            let dailySMABreakdown20Map =
                industrySMABreakdowns20Map 
                |> Map.map (fun industry _ ->
                    let dailyBreakdowns = industry |> Reports.getIndustrySMABreakdownsForDateRange SMA20 dateRange
                    dailyBreakdowns
                )
           
            let title = $"Industry SMA Breakdowns for {formattedDate}"
            
            let infoAndFilterSection = generateInfoAndFilterSection industrySMABreakdowns20Map.Count minimumStocks
            
            let industryTableHeaders = [
                "industry"
                "20 sma %"
                "200 sma %"
                "trend score"
                "cycle score"
                "sma cross over strength"
                "ema cross over strength"
                "adx rank"
                "average above rank"
                "geometric mean rank"
                "weighted rank"
            ]

            let industryRows =
                industrySMABreakdowns200Map.Keys
                |> Seq.map (fun industry ->
                    let industrySMABreakdown20 = industrySMABreakdowns20Map |> Map.find industry
                    let industrySMABreakdown200 = industrySMABreakdowns200Map |> Map.find industry
                    let dailyBreakdowns20 = dailySMABreakdown20Map |> Map.tryFind industry
                    
                    let percentAbove200 = industrySMABreakdown200.breakdown.percentAbove
                    let percentAbove20 = industrySMABreakdown20.breakdown.percentAbove
                    
                    let trendScore, cycleScore, smaCrossOverStrength, emaCrossOverStrength, adxRank =
                        match dailyBreakdowns20 with
                        | None -> 0m, 0m, 0m, 0m, 0m
                        | Some breakdowns ->
                            let trendScore = breakdowns |> MarketCycleScoring.trendScore
                            let cycleScore = breakdowns |> MarketCycleScoring.cycleScore
                            let smaCrossOverStrength = breakdowns |> TrendsCalculator.calculateSMACrossOverStrength
                            let emaCrossOverStrength = breakdowns |> TrendsCalculator.calculateEMACrossOverStrength
                            let adxRank = breakdowns |> TrendsCalculator.calculateADXTrend
                            trendScore, cycleScore, smaCrossOverStrength, emaCrossOverStrength, adxRank
                            
                    let averageAboveRank = (percentAbove20 + percentAbove200) / 2m
                    let geometricMeanRank = System.Math.Sqrt(float (percentAbove20 * percentAbove200)) |> decimal
                    let weightedRank = (percentAbove20 * 0.6m) + (percentAbove200 * 0.4m)
                    
                    tr [] [
                        LinkColumn(industry, industry |> Links.industryLink) |> toTd
                        NumberColumn(percentAbove20) |> toTd
                        NumberColumn(percentAbove200) |> toTd
                        NumberColumn(trendScore) |> toTd
                        NumberColumn(cycleScore) |> toTd
                        NumberColumn(smaCrossOverStrength) |> toTd
                        NumberColumn(emaCrossOverStrength) |> toTd
                        NumberColumn(adxRank) |> toTd
                        NumberColumn(averageAboveRank) |> toTd
                        NumberColumn(geometricMeanRank) |> toTd
                        NumberColumn(weightedRank) |> toTd
                    ]
                )
                |> Seq.toList

            let industryTable = fullWidthTableWithSortableHeaderCells industryTableHeaders industryRows

            let view = toSection title (div [_class "content"] [infoAndFilterSection; industryTable])

            ([view] |> mainLayout $"Industries") next ctx