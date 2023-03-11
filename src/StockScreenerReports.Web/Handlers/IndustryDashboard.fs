namespace StockScreenerReports.Web.Handlers

module IndustryDashboard =
    open StockScreenerReports.Core
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Charts
    open StockScreenerReports.Web.Shared.Views

    let private createEarningsSection industryName = 
        // last 14 days
        let startDate = System.DateTimeOffset.UtcNow.AddDays(-14)
        let endDate = System.DateTimeOffset.UtcNow.AddDays(1)

        let tickersWithEarnings = 
            Reports.getEarningsTickers startDate endDate
            |> List.map (fun (ticker,_) -> ticker)

        let stocksWithEarnings =
            tickersWithEarnings
            |> Storage.getStockByTickers
            |> List.filter (fun s -> s.industry = industryName)

        let newHighs =
            Constants.NewHighsScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
            |> List.map (fun s -> s.ticker)
            |> Set.ofList

        let topGainers = 
            Constants.TopGainerScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
            |> List.map (fun s -> s.ticker)
            |> Set.ofList

        let topLosers =
            Constants.TopLoserScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
            |> List.map (fun s -> s.ticker)
            |> Set.ofList

        let newLows =
            Constants.NewLowsScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
            |> List.map (fun s -> s.ticker)
            |> Set.ofList

        let earningsTableHeader = [ "Ticker"; "Company"; "New High"; "Top Gainer"; "Top Loser"; "New Low"; "Trading View" ]
        let earningsTable =
            stocksWithEarnings
            |> List.map (fun stock ->

                let newHighIcon = newHighs |> Set.contains stock.ticker |> generateNewHighIcon
                let topGainerIcon = topGainers |> Set.contains stock.ticker |> generateTopGainerIcon
                let topLoserIcon = topLosers |> Set.contains stock.ticker |> generateTopLoserIcon
                let newLowIcon = newLows |> Set.contains stock.ticker |> generateNewLowIcon

                tr [] [
                    stock.ticker |> StockTicker.value |> generateTickerLink |> toTdWithNode
                    stock.company |> toTd
                    newHighIcon |> toTdWithNode
                    topGainerIcon |> toTdWithNode
                    topLoserIcon |> toTdWithNode
                    newLowIcon |> toTdWithNode
                    stock.ticker |> StockTicker.value |> Links.tradingViewLink |> generateHrefNewTab "chart" |> toTdWithNode
                ]
            )
            |> fullWidthTableWithSortableHeaderCells earningsTableHeader

        match stocksWithEarnings with
        | [] -> h4 [] ["No earnings last two weeks" |> str]
        | _ -> earningsTable

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
                    screenerResult.date |> Utils.convertToDateString |> toTd
                    (screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags |> toTdWithNode
                    screenerResult.ticker       |> generateTickerLink   |> toTdWithNode
                    screenerResult.marketCap    |> marketCapFormatted   |> toTd
                    screenerResult.price        |> dollarFormatted      |> toTd
                    screenerResult.change       |> percentFormatted     |> toTd
                    screenerResult.volume       |> volumeFormatted      |> toTd
                    screenerResult.ticker       |> Links.tradingViewLink |> generateHrefNewTab "chart" |> toTdWithNode
                ]
            )

        let headerNames = [
            "Date";
            "screener";
            "Ticker";
            "Market Cap";
            "Price";
            "Change";
            "Volume";
            "Trading View"
        ]

        let screenerResultsTable =
            resultRows
            |> fullWidthTableWithSortableHeaderCells headerNames
        
        // get stocks in industry
        let stocks = Storage.getStocksByIndustry industryName

        let stockTableHeaderCells = [
            "Ticker"
            "Company"
            "Sector"
            "Industry"
            "Trading View"
        ]

        let stockTable =
            stocks
            |> List.map (fun stock ->
                tr [] [
                    stock.ticker |> StockTicker.value |> generateTickerLink |> toTdWithNode
                    stock.company |> toTd
                    stock.sector |> Links.sectorLink |> generateHref stock.sector |> toTdWithNode
                    stock.industry |> Links.industryLink |> generateHref stock.industry |> toTdWithNode
                    stock.ticker |> StockTicker.value |> Links.tradingViewLink |> generateHref "chart" |> toTdWithNode
                ]
            )
            |> fullWidthTableWithSortableHeaderCells stockTableHeaderCells

        let topLevel = [
            h1 [] [ str industryName ]
            h5 [] [
                    span [ _class "mx-1"] [
                        industryName 
                        |> Links.industryFinvizLink
                        |> generateHrefNewTab "See it on Finviz"
                    ]
                ]
            breakdownDiv
            trendDiv
        ]

        let earningsSection = createEarningsSection industryName

        let contentSections =
            [screenerChart; earningsSection; screenerResultsTable; stockTable]
            |> List.append (smaBreakdownCharts days)
            |> List.append topLevel

        let view = div [_class "content"] contentSections
        
        [view] |> mainLayout $"{industryName} Industry Dashboard"