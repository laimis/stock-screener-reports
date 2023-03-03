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
        let createBreakdownColumnDiv sma =
            let breakdown = industryName |> Reports.getMostRecentIndustrySMABreakdown sma

            let description =
                match breakdown with
                | None -> "No SMA breakdown found"
                | Some t -> 
                    let total = t.breakdown.above + t.breakdown.below
                    let pct = System.Math.Round((double t.breakdown.above) * 100.0 / (double total), 2)
                    $"<b>{pct}%%</b> ({t.breakdown.above} / {total}) above <b>{t.breakdown.days} SMA</b>"

            div [_class "column"] [rawText description]
            
        let breakdownDiv =
            div [_class "columns"] (
                [20; 200]
                |> List.map createBreakdownColumnDiv
            )

        let createTrendDiv (trend:Option<IndustryTrend>) =
            let desc =
                match trend with
                | None -> "No trend found"
                | Some t -> $"{t.trend}"

            div [ _class "column"] [
                rawText desc
            ]

        let trendDiv = div [_class "columns"] (
            [20;200] |> List.map (fun sma -> 
                let industryTrend = industryName |> Reports.getIndustryTrend sma
                createTrendDiv (industryTrend)
            )
        )

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

        let screenerResultsTable = resultRows |> fullWidthTable tableHeader
        
        // get stocks in industry
        let stocks = Storage.getStocksByIndustry industryName

        let stockTableHeader = tr [] [
            "ticker" |> toSortableHeaderCell
            "company" |> toSortableHeaderCell
            "sector" |> toSortableHeaderCell
            "industry" |> toSortableHeaderCell
            "trading view" |> toSortableHeaderCell
        ]

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
            |> fullWidthTable stockTableHeader

        let view = 
            div [_class "content"] [
                h1 [] [
                    str industryName
                ]
                h5 [] [
                    span [ _class "mx-1"] [industryName |> Links.industryFinvizLink |> generateHrefNewTab "See it on Finviz"]
                ]
                breakdownDiv
                trendDiv
            ]::(smaBreakdownCharts days) @ [screenerChart; screenerResultsTable; stockTable]
        
        view |> mainLayout $"{industryName} Industry Dashboard"