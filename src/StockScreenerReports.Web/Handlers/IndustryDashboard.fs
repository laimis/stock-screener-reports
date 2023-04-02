namespace StockScreenerReports.Web.Handlers

module IndustryDashboard =
    open StockScreenerReports.Core
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Charts
    open StockScreenerReports.Web.Shared.Views
    open FSharp.Data
    open Giraffe
    open StockScreenerReports.Storage.Reports

    type IndustryExportType =   CsvProvider<
        Schema = "ticker, company, sector, industry, country",
        HasHeaders=false>

    let header = "ticker, company, sector, industry, country"

    let private createEarningsSection industryName = 
        // last 14 days
        let duration = 14
        let startDate = System.DateTimeOffset.UtcNow.AddDays(-duration)
        let endDate = System.DateTimeOffset.UtcNow.AddDays(1)
        let dateRangeAsString = (startDate |> Utils.convertToDateStringForOffset),
                                (endDate |> Utils.convertToDateStringForOffset)

        let tickersWithEarnings = 
            getEarningsTickers startDate endDate
            |> List.map (fun (ticker,_) -> ticker)

        let stocksWithEarnings =
            tickersWithEarnings
            |> Storage.getStockByTickers
            |> List.filter (fun s -> s.industry = industryName)

        let newHighs =
            Constants.NewHighsScreenerId
            |> getScreenerResultsForDays dateRangeAsString
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let topGainers = 
            Constants.TopGainerScreenerId
            |> getScreenerResultsForDays dateRangeAsString
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let topLosers =
            Constants.TopLoserScreenerId
            |> getScreenerResultsForDays dateRangeAsString
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let newLows =
            Constants.NewLowsScreenerId
            |> getScreenerResultsForDays dateRangeAsString
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let earningsTableHeader = [ "Ticker"; "Company"; "New High"; "Top Gainer"; "Top Loser"; "New Low"; "Trading View" ]
        let earningsTable =
            stocksWithEarnings
            |> List.map (fun stock ->

                let generateDivWithDateAndIcon iconFunc (screenerResultOption:ScreenerResultReportItem option) =
                    match screenerResultOption with
                    | Some s ->
                        div [] [
                            true |> iconFunc
                            s.date |> Utils.convertToDateString |> str
                        ]
                    | None -> 
                        false |> iconFunc

                let newHighIcon = newHighs |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateNewHighIcon
                let topGainerIcon = topGainers |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateTopGainerIcon
                let topLoserIcon = topLosers |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateTopLoserIcon
                let newLowIcon = newLows |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateNewLowIcon

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
        | _ -> section [] [
            h4 [] [$"Earnings last {duration} days" |> str]
            earningsTable
        ]

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

            let hasTextRight = match sma with | 200 -> "has-text-right" | _ -> ""
            div [_class $"column {hasTextRight}"] [rawText description]
            
        let breakdownSection =
            section [] [
                h4 [] ["SMA Breakdown" |> str]
                div [_class "columns"] (
                    [20; 200]
                    |> List.map createBreakdownColumnDiv
                )
            ]

        let createTrendDiv sma (trend:Option<IndustryTrend>) =
            let desc =
                match trend with
                | None -> "No trend found"
                | Some t -> $"{t.trend}"

            let hasTextRight = match sma with | 200 -> "has-text-right" | _ -> ""

            div [ _class $"column {hasTextRight}"] [
                rawText desc
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
                    title = $"{smaInterval} SMA Trend"
                    color = color
                }

            let datasets = [20; 200] |> List.map createDataset

            let smoothedDataSets = datasets |> Logic.smoothedDataSets 3

            let labels = 
                industryName
                |> Reports.getIndustrySMABreakdownsForIndustry 20 dayOffset
                |> List.map (fun u -> u.breakdown.date.ToString("MMM/dd"))
            
            let charts =
                datasets 
                |> generateChartElements "sma breakdown chart" Line (Some 100) smallChart labels

            let smoothedCharts =
                smoothedDataSets
                |> generateChartElements "sma breakdown chart" Line (Some 100) smallChart labels

            let trendDiv = div [_class "columns"] (
                [20;200] |> List.map (fun sma -> 
                    industryName
                    |> Reports.getIndustryTrend sma
                    |> createTrendDiv sma
                )
            )

            div [] [
                section [] [
                    h4 [] ["SMA Trend Charts" |> str]
                    trendDiv
                    div [] charts
                ]
                section [] [
                    h4 [] ["SMA Trend Windowed (3)" |> str]
                    div [] smoothedCharts
                ]
            ]
            
        
        // load charts for each screener
        let screeners = Storage.getScreeners()

        let days = ReportsConfig.dayRange

        let list = days |> Logic.businessDatesWithZeroPairs

        let labels = list |> List.map (fun (u,_) -> u.ToString("MMM/dd"))

        let datasets = 
            screeners
            |> List.map (fun screener ->
                let dailyCounts = Reports.getDailyCountsForScreenerAndIndustry screener.id industryName days

                let mapped = dailyCounts |> Map.ofList

                let data =
                    list
                    |> List.map(fun (date,_) ->
                        let found = mapped.TryFind date
                        match found with
                        | Some c -> c
                        | None -> 0
                    )
                
                {
                    data = data
                    title = screener.name
                    color = screener.id |> ReportsConfig.getBackgroundColorForScreenerId
                }
            )

        let screenerChart = 
            section [] [
                h4 [ _class "mt-4"] ["Screener Counts" |> str]
                div [] (generateChartElements "screener chart" Bar None smallChart labels datasets)
            ]

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
        let stockTableHeaderCells = [
            "Ticker"
            "Company"
            "Sector"
            "Industry"
            "Country"
            "Trading View"
        ]

        let stocks = industryName |> Storage.getStocksByIndustry
            
        let stockTable =
            stocks
            |> List.sortBy (fun stock -> stock.ticker)
            |> List.map (fun stock ->
                tr [] [
                    stock.ticker |> StockTicker.value |> generateTickerLink |> toTdWithNode
                    stock.company |> toTd
                    stock.sector |> Links.sectorLink |> generateHref stock.sector |> toTdWithNode
                    stock.industry |> Links.industryLink |> generateHref stock.industry |> toTdWithNode
                    stock.country |> Links.countryLink |> generateHref stock.country |> toTdWithNode
                    stock.ticker |> StockTicker.value |> Links.tradingViewLink |> generateHrefNewTab "chart" |> toTdWithNode
                ]
            )
            |> fullWidthTableWithSortableHeaderCells stockTableHeaderCells

        let tickers = stocks |> List.map (fun stock -> stock.ticker |> StockTicker.value)
        let stocksSection = section [_class "mt-5"] [
            h4 [] [
                $"Stocks in Industry ({stocks.Length})" |> str
                small [ _class "is-pulled-right"] [
                    generateHrefWithAttrs
                        "Export"
                        (industryName |> Links.industryExportLink)
                        [(_class "button is-small is-primary") ; (_target "_blank")]
                ]
                small [ _class "is-pulled-right mr-2"] [
                    generateHrefWithAttrs
                        "NGTD Outcomes"
                        ((industryName,tickers,[],"") |> Links.ngtdOutcomesReportLink)
                        [(_class "button is-small is-primary mr-2") ; (_target "_blank")]
                ]
            ]
            stockTable
        ]

        let topLevel = [
            div [ _class "columns"] [
                div [ _class "column"] [
                    h1 [] [ str industryName ]
                ]
                div [ _class "column has-text-right"] [
                    h5 [] [
                        span [ _class "mx-1"] [
                            industryName 
                            |> Links.industryFinvizLink
                            |> generateHrefNewTab "See it on Finviz"
                        ]
                    ]
                ]
            ]
            breakdownSection
        ]

        let earningsSection = createEarningsSection industryName

        let contentSections =
            [(smaBreakdownCharts days); screenerChart; earningsSection; screenerResultsTable; stocksSection]
            |> List.append topLevel

        let view = div [_class "content"] contentSections
        
        [view] |> mainLayout $"{industryName} Industry Dashboard"

    let exportHandler industryName =
        setHttpHeader "Content-Type" "text/csv"
        >=> 
            let stocks = industryName |> Storage.getStocksByIndustry
            let filename = $"export_{industryName}.csv"
            let escapedFilename = System.Uri.EscapeDataString(filename)

            setHttpHeader "Content-Disposition" $"attachment; filename={escapedFilename}"
        >=>
            let rows = 
                stocks
                |> List.sortBy (fun s -> s.ticker)
                |> List.map (fun s -> 
                    IndustryExportType.Row(
                        s.ticker |> StockTicker.value,
                        s.company,
                        s.sector,
                        s.industry,
                        s.country
                    )
                )

            let csv = new IndustryExportType(rows)

            setBodyFromString (header + System.Environment.NewLine + csv.SaveToString())