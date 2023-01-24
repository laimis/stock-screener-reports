namespace StockScreenerReports.Web.Handlers

module IndustryDashboard =
    open StockScreenerReports.Core
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Charts
    open StockScreenerReports.Web.Shared.Views

    let handler industryName =
        
        // load industry trends
        let createBreakdownSpan (breakdown:option<StockScreenerReports.Core.IndustrySMABreakdown>) =
            let desc = 
                match breakdown with
                | None -> "No SMA breakdown found"
                | Some t -> 
                    let total = t.breakdown.above + t.breakdown.below
                    let pct = System.Math.Round((double t.breakdown.above) * 100.0 / (double total), 2)
                    $"<b>{pct}%%</b> ({t.breakdown.above} / {total}) above {t.breakdown.days} SMA"

            span [ _class "mx-1"] [
                rawText desc
            ]
            
        let breakdownDiv = div [] [
            (industryName |> Reports.getMostRecentIndustrySMABreakdown 20 |> createBreakdownSpan)
            (industryName |> Reports.getMostRecentIndustrySMABreakdown 200 |> createBreakdownSpan)
            span [ _class "mx-1"] [industryName |> Links.industryFinvizLink |> generateHrefNewTab "Finviz"]
        ]

        let createTrendSpan trend =
            let desc =
                match trend with
                | None -> "No trend found"
                | Some t ->
                    let pct = System.Math.Round(t.change, 2)
                    $"<b>{pct}</b> change {t.direction} for {t.streak} days"

            span [ _class "mx-1"] [
                rawText desc
            ]

        let trendDiv = div [] [
            industryName |> Reports.getIndustryTrend 20 |> createTrendSpan
            industryName |> Reports.getIndustryTrend 200 |> createTrendSpan
        ]

        let smaBreakdownCharts (dayOffset:int) =
            
            let createDataset smaInterval  : DataSet<decimal> =
                let data =
                    industryName
                    |> Reports.getIndustrySMABreakdownsForIndustry smaInterval dayOffset
                    |> List.map (fun u -> System.Math.Round(u.breakdown.percentAbove, 0))

                let color = 
                    match smaInterval with
                    | 20 -> Constants.ColorRed
                    | _ -> Constants.ColorBlue
                 
                {
                    data = data
                    title = $"{smaInterval} EMA Trend"
                    color = color
                }

            let datasets = [
                createDataset 20;
                createDataset 200
            ]

            let labels = industryName |> Reports.getIndustrySMABreakdownsForIndustry 20 dayOffset |> List.map (fun u -> u.breakdown.date.ToString("MMM/dd"))
            
            generateChartElements "sma breakdown chart" ChartType.Line (Some 100) Charts.smallChart labels datasets
        
        // load charts for each screener
        let screeners = Storage.getScreeners()

        let days = FinvizConfig.dayRange

        let list = days |> Logic.businessDatesWithZeroPairs

        let labels = list |> List.map (fun (u,_) -> u.ToString("MMM/dd"))

        let datasets = 
            screeners
            |> List.map (fun screener ->
                let dailyCounts = Reports.getDailyCountsForScreenerAndIndustry screener.id industryName days

                let mapped = dailyCounts |> Map.ofList

                let data =
                    list
                    |> List.map(fun (date,count) ->
                        let found = mapped.TryFind date
                        match found with
                        | Some c -> c
                        | None -> 0
                    )
                
                {
                    data = data
                    title = screener.name
                    color = screener.id |> FinvizConfig.getBackgroundColorForScreenerId
                }
            )

        let screenerChart = 
            div [ _class "block"] (generateChartElements "screener chart" Bar None smallChart labels datasets)

        let resultRows =
            industryName
            |> Reports.getScreenerResultsForIndustry 50
            |> List.map (fun screenerResult ->
                tr [] [
                    td [] [ screenerResult.date |> Utils.convertToDateString |> str ]
                    td [] [
                        (screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags
                    ]
                    td [] [ screenerResult.ticker |> generateTickerLink ]
                    td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                    td [] [ screenerResult.price |> dollarFormatted |> str ]
                    td [] [ screenerResult.change |> percentFormatted |> str ]
                    td [] [ screenerResult.volume |> volumeFormatted |> str ]
                    td [] [ screenerResult.ticker |> Links.tradingViewLink |> generateHrefNewTab "chart" ]
                ]
            )

        let headerNames = [
            "date";
            "screener";
            "ticker";
            "market cap";
            "price";
            "change";
            "volume";
            "trading view"
        ] 
        
        let tableHeader = tr [] (headerNames |> List.map (fun u -> u |> toSortableHeaderCell))

        let screenerResultsTable = tableHeader::resultRows |> fullWidthTable
        
        // get stocks in industry
        let stocks = Storage.getStocksByIndustry industryName

        let stockTable =
            stocks
            |> List.map (fun stock ->
                tr [] [
                    stock.ticker |> StockTicker.value |> generateTickerLink |> toTdWithNode
                    stock.company |> toTd
                    stock.sector |> Links.sectorLink |> generateHref stock.sector |> toTdWithNode
                    stock.industry |> Links.industryLink |> generateHref stock.industry |> toTdWithNode
                    stock.ticker |> StockTicker.value |> Links.tradingViewLink |> generateHref "Trading View" |> toTdWithNode
                ]
            )
            |> fullWidthTable

        let view = 
            div [_class "content"] [
                h1 [] [
                    str industryName
                ]
                breakdownDiv
                trendDiv
            ]::(smaBreakdownCharts days) @ [screenerChart; screenerResultsTable; stockTable]
        
        view |> mainLayout $"Industry Dashboard for {industryName}" 